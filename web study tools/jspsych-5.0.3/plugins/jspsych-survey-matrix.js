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
     if(window.innerWidth <= 1000) { //&& window.innerHeight <= 800) {
       return true;
     } else {
       return false;
     }
  }

  function detectmob2(){
    if(navigator.platform == "iPhone" || navigator.platform == "iPod" || navigator.platform == "iPad" || navigator.platform == "Android" || navigator.platform == "BlackBerry" || navigator.platform == "Opera"){
      return true;
    } else {
      return false;
    }
  }

  // console.log(window.innerWidth);
  // console.log(window.innerHeight);

    device = detectmob();
    device2 = detectmob2();
    console.log(navigator.platform);

  var plugin = {};

  plugin.info = {
    name: 'survey-matrix',
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
    // html += '<style id="jspsych-survey-matrix-css">';
    // html += ".jspsych-survey-matrix-statement { display:block; font-size: 16px; padding-top: 40px; margin-bottom:10px; }"+
    //   ".jspsych-survey-matrix-opts { list-style:none; width:100%; margin:0; padding:0 0 35px; display:block; font-size: 14px; line-height:1.1em; }"+
    //   ".jspsych-survey-matrix-opt-label { line-height: 1.1em; color: #444; }"+
    //   ".jspsych-survey-matrix-opts:before { content: ''; position:relative; top:11px; /*left:9.5%;*/ display:block; background-color:#efefef; height:4px; width:100%; }"+
    //   ".jspsych-survey-matrix-opts:last-of-type { border-bottom: 0; }"+
    //   ".jspsych-survey-matrix-opts li { display:inline-block; /*width:19%;*/ text-align:center; vertical-align: top; }"+
    //   ".jspsych-survey-matrix-opts li input[type=radio] { display:block; position:relative; top:0; left:50%; margin-left:-6px; }"
    // html += '</style>';
if (device == true || device2 == true) {
    // show preamble text
    html += '<div id="jspsych-survey-likert-preamble" class="jspsych-survey-likert-preamble">'+trial.preamble+'</div>';

    html += '<form id="jspsych-survey-likert-form">';

    // add likert scale questions
    for (var i = 0; i < trial.questions.length; i++) {
      // add question
      html += '<label class="jspsych-survey-likert-statement">' + trial.questions[i].prompt + '</label>';
      // add options
      var width = 100 / trial.questions[i].labels.length;
      var options_string = '<ul class="jspsych-survey-likert-opts" data-radio-group="Q' + i + '">';
      for (var j = 0; j < trial.questions[i].labels.length; j++) {
        options_string += '<li style="width:' + width + '%"><input type="radio" name="Q' + i + '" value="' + j + '"';
        if(trial.questions[i].required){
          options_string += ' required';
        }
        options_string += '><label class="jspsych-survey-likert-opt-label">' + trial.questions[i].labels[j] + '</label></li>';
      }
      options_string += '</ul>';
      html += options_string;
    }
    html += '</tr><tr bgcolor="#778899" class="blank_row"><td class="blank_row" bgcolor="#778899" colspan="2"></td></tr></table>';


    // add submit button
    html += '<button id="jspsych-survey-matrix-next" class="jspsych-survey-matrix jspsych-btn">'+trial.button_label+'</button>';

    html += '</form>'

    display_element.innerHTML = html;

    display_element.querySelector('#jspsych-survey-likert-form').addEventListener('submit', function(e){
      e.preventDefault();
      // measure response time
      var endTime = (new Date()).getTime();
      var response_time = endTime - startTime;

      // create object to hold responses
      var question_data = {};
      var matches = display_element.querySelectorAll('#jspsych-survey-likert-form .jspsych-survey-likert-opts');
      for(var index = 0; index < matches.length; index++){
        var id = matches[index].dataset['radioGroup'];
        var el = display_element.querySelector('input[name="' + id + '"]:checked');
        if (el === null) {
          var response = "";
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
} else{
  // show preamble text
  html += '<div id="jspsych-survey-matrix-preamble" class="jspsych-survey-matrix-preamble">'+trial.preamble+'</div>';

  html += '<form id="jspsych-survey-matrix-form"><table border="0" cellpadding="0" cellspacing="0" align="center">';

  // add matrix scale questions
  for (var i = 0; i < trial.questions.length; i++) {
    if (i == 0) {
      var width = 100 / trial.questions[i].labels.length;
      var tabwidth = trial.questions[i].labels.length * 10;
      var options_string = '<tr><td width:"' + (100-tabwidth) + '%" class="jspsych-survey-matrix-statement cell1"><div>&nbsp;</div></td>' + '<td width:"'+ tabwidth + '%" class="jspsych-survey-matrix-opts cell1" data-radio-group="Q' + i + '"><div style="border: solid 0 #060; border-bottom-width:2px; padding-bottom:0.5ex"><ul class="jspsych-survey-matrix-opts" data-radio-group="Q' + i + '">';
      for (var j = 0; j < trial.questions[i].labels.length; j++) {
        options_string += '<li style="width:' + width + '%"><label class="jspsych-survey-matrix-opt-label">' + trial.questions[i].labels[j] + '</label></li>';
      }
      options_string += '</ul></div></td></tr>';
    } else {
      var width = 100 / trial.questions[i].labels.length;
      var tabwidth = trial.questions[i].labels.length * 10;
      options_string = '<tr><td width:"' + (100-tabwidth) + '%" class="jspsych-survey-matrix-statement cell1">' + trial.questions[(i)].prompt + '</td>' + '<td width:"'+ tabwidth + '%" class="jspsych-survey-matrix-opts cell1" data-radio-group="Q' + (i) + '"><ul class="jspsych-survey-matrix-opts" data-radio-group="Q' + (i) + '">';
      for (var j = 0; j < trial.questions[i].labels.length; j++) {
        options_string += '<li style="width:' + width + '%"><input type="radio" name="Q' + i + '" value="' + j + '"';
        if(trial.questions[i].required){
          options_string += ' required';
        }
        options_string += '></li>';
      }
    }
      options_string += '</ul></td></tr>';
      html += options_string;
    }
    html += '</tr><tr bgcolor="#778899" class="blank_row"><td class="blank_row" bgcolor="#778899" colspan="2"></td></tr></table>';


    // add submit button
    html += '<button id="jspsych-survey-matrix-next" class="jspsych-survey-matrix jspsych-btn">'+trial.button_label+'</button>';

    html += '</form>'

    display_element.innerHTML = html;

    display_element.querySelector('#jspsych-survey-matrix-form').addEventListener('submit', function(e){
      e.preventDefault();
      // measure response time
      var endTime = (new Date()).getTime();
      var response_time = endTime - startTime;

      // create object to hold responses
      var question_data = {};
      var matches = display_element.querySelectorAll('#jspsych-survey-matrix-form .jspsych-survey-matrix-opts');
      for(var index = 0; index < matches.length; index++){
        var id = matches[index].dataset['radioGroup'];
        var el = display_element.querySelector('input[name="' + id + '"]:checked');
        if (el === null) {
          var response = "NA";
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
}
    var startTime = (new Date()).getTime();
  };

  return plugin;
})();
