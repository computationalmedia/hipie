shinyjs.progress = function(params){
  var defaultParams = {id: "", percent: 0};
  params = shinyjs.getParams(params, defaultParams);

  document.getElementById(params.id + "-progressvalue").style = "width:" + params.percent + "%;";
  document.getElementById(params.id + "-progressvalue").innerHTML = params.percent + "%";
}

shinyjs.emptyVideoDataSetCover = function(params) {
  setTimeout(function() {
    // if modal is closed without entering any ids.
    var content = document.getElementById("content");
    content.firstElementChild.style = "visibility: hidden;";
    var warningBox = document.createElement("div");
    warningBox.style = "position: absolute;display: block;top: 10%;";
    warningBox.id = "warningBox";
    var warningText = document.createElement("h2");
    warningText.innerHTML = "This is an empty dataset, please enter an id to plot.";
    var warningButton = document.createElement("button");
    warningButton.className = "btn btn-primary";
    warningButton.innerHTML = "click here to enter";
    var clickFunc = function () {
        $("#youtubeid").modal("show");
    };
    warningButton.onclick = clickFunc;
    warningBox.appendChild(warningText);
    warningBox.appendChild(warningButton);
    content.appendChild(warningBox);
  }, 500)
}

shinyjs.removeCover = function(params) {
  var warningBox = document.getElementById("warningBox");
  if (warningBox) warningBox.parentNode.removeChild(warningBox);
  var content = document.getElementById("content");
  content.firstElementChild.style = "";
}

shinyjs.completeOneVideo = function(params) {
  var defaultParams = {videoId: ""};
  params = shinyjs.getParams(params, defaultParams);

  $("#"+params.videoId+"-progressvalue").addClass('progress-bar-success')
  $("#"+params.videoId+"-progressvalue").removeClass('active')
}

shinyjs.removeOneVideo = function(params) {
  var defaultParams = {videoId: ""};
  params = shinyjs.getParams(params, defaultParams);
  
  $("#"+params.videoId+"-first-row").remove()
  $("#"+params.videoId+"-second-row").remove()
}

shinyjs.showWarning = function(params) {
  var defaultParams = {warning: ""};
  params = shinyjs.getParams(params, defaultParams);
  $("#youtubeidclick").button('reset');
  $('<div id="message" style="position:fixed;top:-30px;left:25%;width:50%;z-index:9999;"><div style="padding: 5px;"><div id="inner-message" class="alert alert-error alert-dismissable" style="margin:0 auto"><a href="#" class="close" data-dismiss="alert" aria-label="close" style="text-decoration: none">×</a>' + params.warning + '</div></div></div>').prependTo('body');
  $('#message').animate({ top: 0 });
  setTimeout(function() {
    $('#message').fadeOut(300);
    setTimeout(function() {
      $('#message').remove();
    }, 300);
  }, 6000)
}

shinyjs.createProgressElement = function(params) {
  var defaultParams = {id: "", title: "", ctitle: "", des: ""};
  params = shinyjs.getParams(params, defaultParams);
  idVal = params.id;
  $("#youtubeidclick").button('reset');
  // create progress element for one video
  var element = document.createElement("div");
  element.className = "row";
  element.id = idVal + "-first-row";
  element.style = "margin: 10px;";
  element.innerHTML = "<div id='"+idVal+"-text' class='col-sm-2 col-md-2 col-lg-2' style='margin-top:10px'></div><div id='"+idVal+"-progress-container' class='col-sm-8 col-md-8 col-lg-8'></div><div id='"+idVal+"-button' class='col-sm-2 col-md-2 col-lg-2' style='margin-top:10px'></div>";
  document.getElementById("progressContainer").appendChild(element);
  // progress bar
  $("<div></div>", {
    "class": "progress",
    style: "margin: 10px;",
    id: idVal + "-progress"
  }).appendTo("#"+idVal+"-progress-container");
  $("<div></div>", {
    "class": "progress-bar progress-bar-striped active",
    role: "progressbar",
    id: idVal + "-progressvalue",
    style: "width:0%;"
  }).appendTo("#" + idVal + "-progress");
  $("<a  id='a-" + idVal +"'data-toggle='collapse' data-target='#" + idVal + "-video,#metadata-" + idVal + "'><i id='"+idVal+"-icon' class='fa fa-chevron-right' aria-hidden='true'></i></a>")
  .click(function(){
    var id = $(this).attr('id').substr(2);
    $("#" + id + "-icon").toggleClass("fa-chevron-right fa-chevron-down");
    setTimeout(function() {
      if ($('#'+id+'-video').hasClass('in')) {
        $("#" + id + "-icon").attr('class', 'fa fa-chevron-down');
      } else {
        $("#" + id + "-icon").attr('class', 'fa fa-chevron-right');
      }
    }, 500)
  }).appendTo("#" + idVal + "-button");
  // cancel button
  $('<i id="b-' + idVal + '"class="fa fa-times-circle-o" aria-hidden="true" style="padding-left: 20px;"></i>').click(function() {
    var id = $(this).attr('id').substr(2);
    Shiny.onInputChange("stopTrainingVideo", id);
  }).appendTo('#' + idVal + '-button');
  // video label
  $("<span class='label label-primary'>"+ idVal+ "</span>").appendTo('#' + idVal + '-text');
  var videoRow = document.createElement("div");
  videoRow.className = "col-sm-7 col-md-7 col-lg-7";
  videoRow.innerHTML = "<div id='"+idVal+"-video' class='collapse'></div>";
  $("<div id='" + idVal + "-second-row' class='row'></div>").appendTo("#progressContainer").append(videoRow)
  $("<div id='metadata-" + idVal + "' class='col-sm-5 col-md-5 col-lg-5 container collapse'></div>").appendTo('#' + idVal + '-second-row');
  $("<div class='row' style='text-overflow: ellipsis;max-height: 20px;word-wrap: break-word;overflow: hidden;'><strong>Youtube ID:</strong> " + idVal + "</div>").appendTo("#metadata-" + idVal)
  $("<div class='row' style='text-overflow: ellipsis;max-height: 40px;word-wrap: break-word;overflow: hidden;'><strong>Title:</strong> " + params.title + "</div>").appendTo("#metadata-" + idVal)
  $("<div class='row' style='text-overflow: ellipsis;max-height: 40px;word-wrap: break-word;overflow: hidden;'><strong>Channel Title:</strong> " + params.ctitle + "</div>").appendTo("#metadata-" + idVal)
  $("<div class='row' style='text-overflow: ellipsis;max-height: 120px;word-wrap: break-word;overflow: hidden;'><strong>Description:</strong> " + params.des + "</div>").appendTo("#metadata-" + idVal)
  // creating preview iframe
  var url = "https://www.youtube.com/embed/" + idVal;
  $('<iframe src="'+ url +'" width="300" height="200" frameborder="0" allowfullscreen></iframe>').appendTo("#"+idVal+"-video");
  
}