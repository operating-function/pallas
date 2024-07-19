(() => {
  var getJSON = function(url, callback) {
      var xhr = new XMLHttpRequest();
      xhr.open('GET', url, true);
      xhr.responseType = 'json';
      xhr.onload = function() {
        var status = xhr.status;
        if (status === 200) {
          callback(null, xhr.response);
        } else {
          callback(status, xhr.response);
        }
      };
      xhr.send();
  };

  var changePlayerSrcTo = function(src) {
    return function() {
      document.getElementById('player').innerHTML =
        '<video height="800" autoplay controls id="video-ctrl"><source src="'
        +src+'" type="video/mp4"></video>';
      document.getElementById('video-ctrl').play();
    };
  };

  getJSON('/file', function (err, data) {
    if (err !== null) {
      console.error('something went wrong: ' + err);
    } else {
      var listDiv = document.getElementById('vid-list');
      var ul=document.createElement('ul');
      listDiv.appendChild(ul);

      for (var i = 0; i < data.length; ++i) {
        var button = document.createElement("input");
        button.type = "button";
        button.value = data[i];
        button.onclick = changePlayerSrcTo(data[i]);
        var li=document.createElement('li');
        li.appendChild(button);
        ul.appendChild(li);
      }
    }
  });
})();
