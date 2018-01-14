/**
 * jspsych-survey-matrix
 * a jspsych plugin for measuring items on a matrix scale
 *
 * Josh de Leeuw
 *
 * documentation: docs.jspsych.org
 *
 */

jsPsych.plugins['survey-matrix'] = (function() {

  function detectmob() {
   if(window.innerWidth <= 800) { //&& window.innerHeight <= 800) {
     return true;
   } else {
     return false;
   }
}

console.log(window.innerWidth);
console.log(window.innerHeight);

  device = detectmob();
  console.log(device);

  var plugin = {};

  plugin.trial = function(display_element, trial) {

    // default parameters for the trial
    trial.preamble = typeof trial.preamble === 'undefined' ? "" : trial.preamble;

    // if any trial variables are functions
    // this evaluates the function and replaces
    // it with the output of the function
    trial = jsPsych.pluginAPI.evaluateFunctionParameters(trial);


if (device == true) {
  // show preamble text
  display_element.append($('<div>', {
    "id": 'jspsych-survey-likert-preamble',
    "class": 'jspsych-survey-likert-preamble'
  }));

  $('#jspsych-survey-likert-preamble').html(trial.preamble);

  display_element.append('<form id="jspsych-survey-likert-form">');
  // add likert scale questions
  for (var i = 0; i < trial.questions.length; i++) {
    form_element = $('#jspsych-survey-likert-form');
    // add question
    form_element.append('<label class="jspsych-survey-likert-statement">' + trial.questions[i] + '</label>');
    // add options
    var width = 100 / trial.labels[i].length;
    options_string = '<ul class="jspsych-survey-likert-opts" data-radio-group="Q' + i + '">';
    for (var j = 0; j < trial.labels[i].length; j++) {
      options_string += '<li style="width:' + width + '%"><input type="radio" name="Q' + i + '" value="' + j + '"><label class="jspsych-survey-likert-opt-label">' + trial.labels[i][j] + '</label></li>';
    }
    options_string += '</ul>';
    form_element.append(options_string);
  }
} else {
  // show preamble text
  display_element.append($('<div>', {
    "id": 'jspsych-survey-matrix-preamble',
    "class": 'jspsych-survey-matrix-preamble'
  }));

  $('#jspsych-survey-matrix-preamble').html(trial.preamble);

  display_element.append('<form id="jspsych-survey-matrix-form">');
  // add matrix scale questions
  form_element = $('#jspsych-survey-matrix-form');
  form_element.append('<table>')
  // add options
  for (var i = 0; i <= trial.questions.length; i++) {
    // add question
    if (i == 0) {
      var width = 100 / trial.labels[i].length;
      options_string = '<tr><td width:"50%" class="jspsych-survey-matrix-statement cell1"><div>&nbsp;</div></td>' + '<td width:"50%" class="jspsych-survey-matrix-opts cell1" data-radio-group="Q' + i + '"><div style="border: solid 0 #060; border-bottom-width:2px; padding-bottom:0.5ex"><ul class="jspsych-survey-matrix-opts" data-radio-group="Q' + i + '">';
      for (var j = 0; j < trial.labels[i].length; j++) {
        options_string += '<li style="width:' + width + '%"><label class="jspsych-survey-matrix-opt-label">' + trial.labels[i][j] + '</label></li>';
      }
      options_string += '</ul></div></td></tr>';
    } else {
      var width = 100 / trial.labels[i-1].length;
      options_string = '<tr><td width:"50%" class="jspsych-survey-matrix-statement cell1">' + trial.questions[(i-1)] + '</td>' + '<td width:"50%" class="jspsych-survey-matrix-opts cell1" data-radio-group="Q' + (i-1) + '"><ul class="jspsych-survey-matrix-opts" data-radio-group="Q' + (i-1) + '">';
      for (var j = 0; j < trial.labels[(i-1)].length; j++) {
        options_string += '<li style="width:' + width + '%"><input type="radio" name="Q' + (i-1) + '" value="' + j + '"></li>';
      }
    }
    options_string += '</ul></td></tr>';
    // add options
    form_element.append(options_string);
  }
  form_element.append('</tr><tr bgcolor="#002A5C" class="blank_row"><td class="blank_row" bgcolor="#002A5C" colspan="2"></td></tr></table>')
}

    // add submit button
    display_element.append($('<button>', {
      'id': 'jspsych-survey-likert-next',
      'class': 'jspsych-survey-likert-next jspsych-btn'
    }));
    $("#jspsych-survey-likert-next").html('Submit Answers');
    $("#jspsych-survey-likert-next").click(function() {
      // measure response time
      var endTime = (new Date()).getTime();
      var response_time = endTime - startTime;

if (device == true) {
  // create object to hold responses
  var question_data = {};
  $("#jspsych-survey-likert-form .jspsych-survey-likert-opts").each(function(index) {
    var id = $(this).data('radio-group');
    var response = $('input[name="' + id + '"]:checked').val();
    if (typeof response == 'undefined') {
      response = "NA";
    }
    var obje = {};
    obje[id] = response;
    $.extend(question_data, obje);
  });
} else {
  // create object to hold responses
  var question_data = {};
  $("#jspsych-survey-matrix-form .jspsych-survey-matrix-opts").each(function(index) {
    var id = $(this).data('radio-group');
    var response = $('input[name="' + id + '"]:checked').val();
    if (typeof response == 'undefined') {
      response = "NA";
    }
    var obje = {};
    obje[id] = response;
    $.extend(question_data, obje);
  });
}



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
