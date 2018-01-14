/**
 * jspsych-survey-text
 * a jspsych plugin for free response survey questions
 *
 * Josh de Leeuw
 *
 * documentation: docs.jspsych.org
 *
 */


jsPsych.plugins['survey-text'] = (function() {

  var plugin = {};

  plugin.info = {
    name: 'survey-text',
    description: '',
    parameters: {
      questions: {
        type: jsPsych.plugins.parameterType.COMPLEX,
        array: true,
        pretty_name: 'Questions',
        nested: {
          prompt: {type: jsPsych.plugins.parameterType.STRING,
                   pretty_name: 'Prompt',
                   default: undefined,
                   description: 'Prompts for the the subject to response'},
          value: {type: jsPsych.plugins.parameterType.STRING,
                  pretty_name: 'Value',
                  array: true,
                  default: '',
                  description: 'The strings will be used to populate the response fields with editable answers.'},
          rows: {type: jsPsych.plugins.parameterType.INT,
                 pretty_name: 'Rows',
                 array: true,
                 default: 1,
                 description: 'The number of rows for the response text box.'},
          columns: {type: jsPsych.plugins.parameterType.INT,
                    pretty_name: 'Columns',
                    array: true,
                    default: 40,
                    description: 'The number of columns for the response text box.'}
        }
      },
      preamble: {
        type: jsPsych.plugins.parameterType.STRING,
        pretty_name: 'Preamble',
        default: '',
        description: 'HTML formatted string to display at the top of the page above all the questions.'
      },
      button_label: {
        type: jsPsych.plugins.parameterType.STRING,
        pretty_name: 'Button label',
        default: 'Next',
        description: 'The text that appears on the button to finish the trial.'
      }
    }
  }

  plugin.trial = function(display_element, trial) {

    // console.log(trial.questions.rows);

    if (typeof trial.questions.rows == 'undefined') {
      trial.questions.rows = [];
      for (var i = 0; i < trial.questions.length; i++) {
        trial.rows.push(1);
        console.log(trial.rows.push(1));
      }
    }
    if (typeof trial.questions.columns == 'undefined') {
      trial.questions.columns = [];
      for (var i = 0; i < trial.questions.length; i++) {
        trial.columns.push(1);
      }
    }
    if (typeof trial.questions.value == 'undefined') {
      trial.questions.value = [];
      for (var i = 0; i < trial.questions.length; i++) {
        trial.questions.value.push("");
      }
    }

    // show preamble text
    var html = '<div id="jspsych-survey-text-preamble" class="jspsych-survey-text-preamble">'+trial.preamble+'</div>';

    console.log(trial.questions);

    // add questions
    var i = 0;
      html += '<div id="jspsych-survey-text-"'+i+'" class="jspsych-survey-text-question" style="margin: 2em 0em;">';
      html += '<p class="jspsych-survey-text">' + trial.questions.prompt + '</p>';
        console.log("yup");
      if(trial.questions.rows == 1){
        html += '<input type="text" name="#jspsych-survey-text-response-' + i + '" size="'+trial.questions.columns+'">'+trial.questions.value+'</input>';
      } else {
        html += '<textarea name="#jspsych-survey-text-response-' + i + '" cols="' + trial.questions.columns + '" rows="' + trial.questions.rows + '">'+trial.questions.value+'</textarea>';
      }
      html += '</div>';


    // add submit button
    html += '<button id="jspsych-survey-text-next" class="jspsych-btn jspsych-survey-text">'+trial.button_label+'</button>';

    display_element.innerHTML = html;

    display_element.querySelector('#jspsych-survey-text-next').addEventListener('click', function() {
      // measure response time
      var endTime = (new Date()).getTime();
      var response_time = endTime - startTime;

      // create object to hold responses
      var question_data = {};
      var matches = display_element.querySelectorAll('div.jspsych-survey-text-question');
      for(var index=0; index<matches.length; index++){
        var id = "Q" + index;
        var val = matches[index].querySelector('textarea, input').value;
        var obje = {};
        obje[id] = val;
        Object.assign(question_data, obje);
      }
      // save data
      var trialdata = {
        "rt": response_time,
        "responses": JSON.stringify(question_data)
      };

      display_element.innerHTML = '';

      // next trial
      jsPsych.finishTrial(trialdata);
    });

    var startTime = (new Date()).getTime();
  };

  return plugin;
})();
