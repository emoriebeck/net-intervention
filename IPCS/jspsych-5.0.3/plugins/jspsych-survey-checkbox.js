/**
 * jspsych-survey-checkbox
 * a jspsych plugin for measuring items on a checkbox scale
 *
 * Josh de Leeuw
 *
 * documentation: docs.jspsych.org
 *
 */

jsPsych.plugins['survey-checkbox'] = (function() {

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

  plugin.info = {
    name: 'survey-checkbox',
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
                     description: 'Questions that are associated with the slider.'},
          labels: {type: jsPsych.plugins.parameterType.STRING,
                   array: true,
                   pretty_name: 'Labels',
                   default: undefined,
                   description: 'Labels to display for individual question.'},
          required: {type: jsPsych.plugins.parameterType.BOOL,
                     pretty_name: 'Required',
                     default: false,
                     description: 'Makes answering questions required.'}
        }
      },
      preamble: {
        type: jsPsych.plugins.parameterType.STRING,
        pretty_name: 'Preamble',
        default: '',
        description: 'String to display at top of the page.'
      },
      button_label: {
        type: jsPsych.plugins.parameterType.STRING,
        pretty_name: 'Button label',
        default: 'Next',
        description: 'Label of the button.'
      }
    }
  }

  plugin.trial = function(display_element, trial) {


    var html = "";
    // inject CSS for trial
    // html += '<style id="jspsych-survey-checkbox-css">';
    // html += ".jspsych-survey-checkbox-statement { display:block; font-size: 16px; padding-top: 40px; margin-bottom:10px; }"+
    //   ".jspsych-survey-checkbox-opts { list-style:none; width:100%; margin:0; padding:0 0 35px; display:block; font-size: 14px; line-height:1.1em; }"+
    //   ".jspsych-survey-checkbox-opt-label { line-height: 1.1em; color: #444; }"+
    //   ".jspsych-survey-checkbox-opts:before { content: ''; position:relative; top:11px; /*left:9.5%;*/ display:block; background-color:#efefef; height:4px; width:100%; }"+
    //   ".jspsych-survey-checkbox-opts:last-of-type { border-bottom: 0; }"+
    //   ".jspsych-survey-checkbox-opts li { display:inline-block; /*width:19%;*/ text-align:center; vertical-align: top; }"+
    //   ".jspsych-survey-checkbox-opts li input[type=radio] { display:block; position:relative; top:0; left:50%; margin-left:-6px; }"
    // html += '</style>';
if (device == true){
  html += '<div id="jspsych-survey-checkbox-preamble" class="jspsych-survey-checkbox-preamble">'+trial.preamble+'</div>';

  html += '<form id="jspsych-survey-checkbox-form">';

  html += '<table border="0" cellpadding="0" cellspacing="0" align="center" style:"margin:4px padding-top:20px">'
  html += '</tr><tr bgcolor="#002A5C" class="blank_row"><td class="blank_row" bgcolor="#002A5C" colspan="2"></td></tr>';
  // add checkbox scale questions
  for (var i = 0; i < trial.questions.length; i++) {
    var j = 1
    // add question
    html += '<tr><td style="width:80%" class="jspsych-survey-checkbox-statement cell1">' + trial.questions[i].prompt + '</td>' + '<td style="width:5%" class="cell1"> </td>' + '<td style="width:15%" class="jspsych-survey-checkbox-opts cell1" data-radio-group="Q' + i + '"> <input type="checkbox" class="jspsych-survey-checkbox-opts cell1" name="Q' + i + '" value="' + j + '"></td></tr>'
    html += '<tr bgcolor="#002A5C" class="blank_row"><td class="blank_row" bgcolor="#002A5C" colspan="2"></td></tr>';
    // add options
    console.log(trial.questions[i]);
    // var options_string = '<ul class="jspsych-survey-checkbox-opts" data-radio-group="Q' + i + '">';
      if(trial.questions[i].required){
        // options_string += ' required';
      }
      // options_string += '><label class="jspsych-survey-checkbox-opt-label">' + trial.questions[i].labels[j] + '</label></li>';
    // options_string += '</ul>';
    // html += options_string;
  }

  html += '</tr><tr bgcolor="#002A5C" class="blank_row"><td class="blank_row" bgcolor="#002A5C" colspan="2"></td></tr>';
  html += '</tr><tr bgcolor="#002A5C" class="blank_row"><td class="blank_row" bgcolor="#002A5C" colspan="2"></td></tr></table>';
} else{
    // show preamble text
    html += '<div id="jspsych-survey-checkbox-preamble" class="jspsych-survey-checkbox-preamble">'+trial.preamble+'</div>';

    html += '<form id="jspsych-survey-checkbox-form">';

    html += '<table border="0" cellpadding="0" cellspacing="0" align="center" style:"margin:4px padding-top:20px">'
    html += '<tr bgcolor="#FAF0E6" class="blank_row"><td class="blank_row" bgcolor="#FAF0E6" colspan="5"></td></tr>';
    // add checkbox scale questions
    for (var i = 0; i < trial.questions.length; i++) {
      var j = 1
      // add question
      html += '<tr><td style="width:5%" class="cell1"></td><td style="width:75%" class="jspsych-survey-checkbox-statement cbp">' + trial.questions[i].prompt + '</td>' + '<td style="width:5%" class="cell1"> </td>' + '<td style="width:10%" class="jspsych-survey-checkbox-opts cell1" data-radio-group="Q' + i + '"> <input type="checkbox" class="jspsych-survey-checkbox-opts cell1" name="Q' + i + '" value="' + j + '"data-radio-group="Q' + i + '"></td><td style="width:5%" class="cell1"></td></tr>'

      // add options
      // console.log(trial.questions[i]);
      // var options_string = '<ul class="jspsych-survey-checkbox-opts" data-radio-group="Q' + i + '">';
        if(trial.questions[i].required){
          // options_string += ' required';
        }
        // options_string += '><label class="jspsych-survey-checkbox-opt-label">' + trial.questions[i].labels[j] + '</label></li>';
      // options_string += '</ul>';
      // html += options_string;
    }

    html += '</tr><tr bgcolor="#FAF0E6" class="blank_row"><td class="blank_row" bgcolor="#FAF0E6" colspan="5"></td></tr>';
    html += '</tr><tr bgcolor="#778899" class="blank_row"><td class="blank_row" bgcolor="#778899" colspan="5"></td></tr></table>';
}
    // add submit button
    html += '<button id="jspsych-survey-checkbox-next" class="jspsych-survey-checkbox jspsych-btn">'+trial.button_label+'</button>';

    html += '</form>'

    display_element.innerHTML = html;

    display_element.querySelector('#jspsych-survey-checkbox-form').addEventListener('submit', function(e){
      e.preventDefault();
      // measure response time
      var endTime = (new Date()).getTime();
      var response_time = endTime - startTime;

      // create object to hold responses
      var question_data = {};
      var matches = display_element.querySelectorAll('#jspsych-survey-checkbox-form .jspsych-survey-checkbox-opts');
      console.log(matches);
      for(var index = 0; index < matches.length; index++){
        var id = matches[index].dataset['radioGroup'];
        console.log(id);
        var el = display_element.querySelector('input[name="' + id + '"]:checked');
        if (el === null) {
          var response = "0";
        } else {
          var response = parseInt(el.value);
        }
        var obje = {};
        obje[id] = response;
        Object.assign(question_data, obje);
      }

      // save data
      var trial_data = {
        "rt": response_time,
        "responses": JSON.stringify(question_data)
      };

      display_element.innerHTML = '';

      // next trial
      jsPsych.finishTrial(trial_data);
    });

    var startTime = (new Date()).getTime();
  };

  return plugin;
})();
