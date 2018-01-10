<script>
window.document.getElementsByClassName("sidebar-toggle")[0].onclick = function () {
  Shiny.onInputChange("resize", Math.random());
};

// detect if it's mobile
var isMobile = {
    Android: function() {
        return navigator.userAgent.match(/Android/i);
    },
    BlackBerry: function() {
        return navigator.userAgent.match(/BlackBerry/i);
    },
    iOS: function() {
        return navigator.userAgent.match(/iPhone|iPad|iPod/i);
    },
    Opera: function() {
        return navigator.userAgent.match(/Opera Mini/i);
    },
    Windows: function() {
        return navigator.userAgent.match(/IEMobile/i);
    },
    any: function() {
        return (isMobile.Android() || isMobile.BlackBerry() || isMobile.iOS() || isMobile.Opera() || isMobile.Windows());
    }
};
if( isMobile.any() ) {
  $('body')[0].className = '';
  $('.wrapper')[0].innerHTML = 'Sorry, this demo doesn\'t support mobile yet. Please visit hipdemo.ml from desktops or click <a href="https://github.com/andrei-rizoiu/hip-popularity">here</a> to learn more about HIP.';
}

$(document).on("shiny:connected", function () {
  // add about hipdemo button
  $('<a href="https://github.com/andrei-rizoiu/hip-popularity" target="_blank" style="position:fixed;z-index: 999;left: 20px; bottom: 10px; color: white; font-size: 20px;">About HIPDEMO</a>').appendTo('body');
  
  // cuz menu items are renderred from server, need to delay the atachment of tab changing listener
  initfunc = function() {
    if (window.document.getElementById('menu').childNodes[1] == undefined) {
      setTimeout(initfunc, 500);
      return;
    }
    window.document.getElementById('menu').childNodes[1].className = "active";
    Shiny.onInputChange("chosentab", window.document.getElementsByClassName("active")[0].childNodes[1].dataset.value);
    // listener for tab changing
    window.document.getElementsByClassName('sidebar-menu')[0].onclick = function() {
      setTimeout(function(){
        let chosen = 0;
        for (let i = 0; i < $('li').length; i++) {
        	if ($('li')[i].className === 'active')
        		chosen = i;
        }
        Shiny.onInputChange("chosentab", chosen);
      }, 100);
    }
    // adding dataset modification buttons
    $('<div id="datasetButtonGroup" class="btn-group" role="group" style="margin:15px;"></div>').appendTo('.sidebar');
    // adding a new Add Dataset button
    $('<button id="adddataset" class="btn btn-primary"  data-toggle="tooltip" data-placement="top" title="" data-original-title="Add a new dataset"><i class="fa fa-plus" aria-hidden="true"></i></button>').click(function() {
    	$("#datasetButtonGroup").hide();
    	$('<form><input id="datasetname" class="form-control" type="text" placeholder="New dataset name" style="margin:15px; width:180px;"></input></form>').keyup(function(event) {
    		event.preventDefault();
    		if (event.keyCode == 13) {
    			var name = $('#datasetname').val();
    			$('#datasetname').remove();
    			Shiny.onInputChange("newDatasetName", name);
    			$("#datasetButtonGroup").show();
        } else if (event.keyCode == 27) {
          $('#datasetname').remove();
          $("#datasetButtonGroup").show();
        }
    	}).appendTo('.sidebar');
    }).appendTo('#datasetButtonGroup');
    // adding dataset modify button
    $('<button id="adddataset" class="btn btn-default"  data-toggle="tooltip" data-placement="top" title="" data-original-title="Change current dataset name"><i class="fa fa-cog" aria-hidden="true"></i></button>').click(function() {
      var tmp = $('.active').children().children();
      $('.active').children().children().replaceWith($('<form><input id="datasetname" class="form-control" type="text" placeholder="New dataset name" style="width:180px;"></input></form>').keyup(function(event) {
        event.preventDefault();
    		if (event.keyCode == 13) {
    			var name = $('#datasetname').val();
    			tmp.text(name);
    			$('#datasetname').replaceWith(tmp);
    			Shiny.onInputChange("changeDatasetName", name);
        } else if (event.keyCode == 27) {
          $('#datasetname').replaceWith(tmp);
        }
      }));
    }).appendTo('#datasetButtonGroup');
    // adding delete dataset button
    $('<button id="adddataset" class="btn btn-danger"  data-toggle="tooltip" data-placement="top" title="" data-original-title="Delete this dataset"><i class="fa fa-minus-circle" aria-hidden="true"></i></button>').click(function() {
      if ($('.active').children().data().value <= 4) {
        shinyjs.showWarning({warning: "Cannot delete default dataset!"})
      } else {
        Shiny.onInputChange("deleteDataset", $('.active').children().data().value);
        $('.active').remove()
        $('a[data-value="0"]').parent().addClass('active');
        $('.sidebar-menu').trigger('click');
      }
      
    }).appendTo('#datasetButtonGroup');
  }
  initfunc();
  //Shiny.onInputChange("height", window.document.getElementsByClassName("content")[0].offsetHeight);
  
  // set click event for a button to invoke the modal
  var onTheFlyButton = document.createElement("button");
  onTheFlyButton.className = "btn btn-primary";
  onTheFlyButton.type = "button";
  onTheFlyButton.innerHTML = '<i class="fa fa-plus" aria-hidden="true"></i> Add New Video To This Dataset';
  onTheFlyButton.onclick = function () {
    $("#youtubeid").modal("show");
  };
  var ul = document.createElement("ul");
  ul.className = "nav navbar-nav navbar-right";
  ul.style = "padding:10px;margin-right:20px;";
  document.getElementsByTagName("nav")[0].appendChild(ul);
  // remove video button
  $('<div class="btn-group" role="group"></div>')
    .appendTo(ul)
    .append(onTheFlyButton)
    .append($('<button class="btn btn-danger" role="button" id="removevideo"><i class="fa fa-minus-circle" aria-hidden="true"></i> Remove Current Video From Dataset</button>').click(function() {
    Shiny.onInputChange("removeCurrentVideo", $('a[target="_blank"')[2].innerHTML);
  }))
  // on button created event
  Shiny.onInputChange("onAddVideoButtonCreated", Math.random());
  
  // add search form
  $('<form class="navbar-form navbar-left" style="min-width:50%;"><div class="form-group" style="width:45%;"><input type="text" id="searchForm" class="form-control" placeholder="Search this dataset in id, title, author, description, category" style="width: 100%;"></div><button id="search" type="submit" class="btn btn-default" style="display: inline-block;position: relative;"><i class="fa fa-search" aria-hidden="true"></button></form>').appendTo('.navbar')
  $('#search').click(function() {
    Shiny.onInputChange("search", $('#searchForm').val() + ' ' + Math.random().toString().substr(3,3));
    if ($('#cancelSearch').length === 0) {
      $('<i id="cancelSearch" class="fa fa-times" aria-hidden="true" style="position: relative;margin: 10px -20px;"></i>')
      .click(function() {
        Shiny.onInputChange("cancelSearch", Math.random());
        $('#searchForm').val('');
        $(this).remove();
      }).appendTo('.form-group');
    }
  })
  // get youtubeid when clicking Add!
  $("#youtubeidclick")[0].onclick = function () {
    var idVal = $("#idinput").val();
    if (idVal !== "") {
        $("#youtubeidclick").button('loading');
        $("#idinput").val('');
        Shiny.onInputChange("inputID", idVal + ' ' + Math.random().toString().substr(3,3));
    } else {
      shinyjs.showWarning({warning: "Please enter a valid youtube video id."});
    }
  };

  // for resizing
  $(window).resize(function(e) {
    Shiny.onInputChange("resize", Math.random());
  });
});
</script>