/**
 * jspsych-survey-likert
 * a jspsych plugin for measuring items on a likert scale
 *
 * Josh de Leeuw
 *
 * documentation: docs.jspsych.org
 *
 */

jsPsych.plugins['survey-checkbox'] = (function() {

  var plugin = {};

  plugin.trial = function(display_element, trial) {

    // default parameters for the trial
    trial.preamble = typeof trial.preamble === 'undefined' ? "" : trial.preamble;

    // if any trial variables are functions
    // this evaluates the function and replaces
    // it with the output of the function
    trial = jsPsych.pluginAPI.evaluateFunctionParameters(trial);

    // show preamble text
    display_element.append($('<div>', {
      "id": 'jspsych-survey-checkbox-preamble',
      "class": 'jspsych-survey-checkbox-preamble'
    }));

    $('#jspsych-survey-checkbox-preamble').html(trial.preamble);

    display_element.append('<form id="jspsych-survey-checkbox-form">');
    // add likert scale questions
    form_element = $('#jspsych-survey-checkbox-form');
    form_element.append('<table style:"padding-top:20px">')
    for (var i = 0; i < trial.questions.length; i++) {
      // add question
      var j = 1
      form_element.append('<tr><td style="width:90%" class="jspsych-survey-checkbox-statement">' + trial.questions[i] + '</td>' + '<td style="width:15%" class="jspsych-survey-checkbox-opts" data-radio-group="Q' + i + '"> <input type="radio" name="Q' + i + '" value="' + j + '"></td></tr><br><br><br>');
      // add options
      options_string = '<ul class="jspsych-survey-checkbox-opts" data-radio-group="Q' + i + '">';
    }
    form_element.append('</tr><tr bgcolor="#002A5C" class="blank_row">&nbsp<td class="blank_row" bgcolor="#002A5C" colspan="2"></td></tr></table>')

    // add submit button
    display_element.append($('<button>', {
      'id': 'jspsych-survey-checkbox-next',
      'class': 'jspsych-survey-checkbox jspsych-btn'
    }));
    $("#jspsych-survey-checkbox-next").html('Submit Answers');
    $("#jspsych-survey-checkbox-next").click(function() {
      // measure response time
      var endTime = (new Date()).getTime();
      var response_time = endTime - startTime;

      // create object to hold responses
      var question_data = {};
      $("#jspsych-survey-checkbox-form .jspsych-survey-checkbox-opts").each(function(index) {
        var id = $(this).data('radio-group');
        var response = $('input[name="' + id + '"]:checked').val();
        if (typeof response == 'undefined') {
          response = 0;
        }
        var obje = {};
        obje[id] = response;
        $.extend(question_data, obje);
      });

      // save data
      var trial_data = {
        "rt": response_time,
        "responses": JSON.stringify(question_data)
      };

      display_element.html('');

      // next trial
      jsPsych.finishTrial(trial_data);
    });

    var startTime = (new Date()).getTime();
  };

  return plugin;
})();
