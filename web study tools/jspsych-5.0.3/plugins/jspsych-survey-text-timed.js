/**
 * jspsych-survey-text-timed
 * a jspsych plugin for free response survey questions
 *
 * Josh de Leeuw
 *
 * documentation: docs.jspsych.org
 *
 */


jsPsych.plugins['survey-text-timed'] = (function() {

  var plugin = {};

  plugin.info = {
    name: 'survey-text-timed',
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
      },
      trial_duration: {
        type: jsPsych.plugins.parameterType.INT,
        pretty_name: 'Trial duration',
        default: -1,
        description: 'How long to show the trial.'
      }
    }
  }

  plugin.trial = function(display_element, trial) {

    // console.log(trial.questions.rows);

    if (typeof trial.questions.rows == 'undefined') {
      trial.questions.rows = [];
      for (var i = 0; i < trial.questions.length; i++) {
        trial.rows.push(1);
        // console.log(trial.rows.push(1));
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
    var html = '<div id="jspsych-survey-text-timed-preamble" class="jspsych-survey-text-timed-preamble">'+trial.preamble+'</div>';

    // console.log(trial.questions);
    console.log(trial.trial_duration);

    // add questions
    var i = 0;
      html += '<div id="jspsych-survey-text-timed-"'+i+'" class="jspsych-survey-text-timed-question" style="margin: 2em 0em;">';
      html += '<p class="jspsych-survey-text-timed">' + trial.questions.prompt + '</p>';
        // console.log("yup");
      if(trial.questions.rows == 1){
        html += '<input type="text" name="#jspsych-survey-text-timed-response-' + i + '" size="'+trial.questions.columns+'">'+trial.questions.value+'</input>';
      } else {
        html += '<textarea name="#jspsych-survey-text-timed-response-' + i + '" cols="' + trial.questions.columns + '" rows="' + trial.questions.rows + '">'+trial.questions.value+'</textarea>';
      }
      html += '</div>';


    // add submit button
    html += '<button id="jspsych-survey-text-timed-next" class="jspsych-btn jspsych-survey-text-timed">'+trial.button_label+'</button>';

    display_element.innerHTML = html;

    display_element.querySelector('#jspsych-survey-text-timed-next').addEventListener('click', function() {
      // measure response time
      var endTime = (new Date()).getTime();
      var response_time = endTime - startTime;
      jsPsych.pluginAPI.clearAllTimeouts();
      // create object to hold responses
      var question_data = {};
      var matches = display_element.querySelectorAll('div.jspsych-survey-text-timed-question');
      console.log(matches);
      for(var index=0; index<matches.length; index++){
        var id = "Q" + index;
        var val = matches[index].querySelector('textarea, input').value;
        if (val == ""){
          var val = "NA"
        }
        console.log(val);
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

    // end trial if time limit is set
    if (trial.trial_duration > 0) {
      jsPsych.pluginAPI.setTimeout(function() {
        end_trial();
      }, trial.trial_duration);
    }
    function end_trial() {

      // kill any remaining setTimeout handlers
      jsPsych.pluginAPI.clearAllTimeouts();
      var response_time = trial.trial_duration;

      var question_data = {};
      var matches = display_element.querySelectorAll('div.jspsych-survey-text-timed-question');
      console.log(matches);
      for(var index=0; index<matches.length; index++){
        var id = "Q" + index;
        var val = matches[index].querySelector('textarea, input').value;
        if (val == ""){
          var val = "timeout"
        }
        var obje = {};
        obje[id] = val;
        Object.assign(question_data, obje);
      }
      // save data
      var trial_data = {
        "rt": response_time,
        "responses": JSON.stringify(question_data)
      };
      display_element.innerHTML = '';
      // move on to the next trial
      jsPsych.finishTrial(trial_data);

    };

  };

  // function to end trial when it is time

  // clear the display

  // kill any remaining setTimeout handlers
  return plugin;
})();
