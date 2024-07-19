(() => {
  let httpRequest;
  document
    .getElementById("goButton")
    .addEventListener("click", makeRequest);

  function makeRequest() {
    const canvas = document.querySelector("canvas");
    var xhr = new XMLHttpRequest();
    xhr.open("POST", "/fract", true);
    xhr.setRequestHeader("Content-Type", "application/json");
    xhr.responseType = "arraybuffer";
    xhr.onreadystatechange = function () {
        if (xhr.readyState === 4 && xhr.status === 200) {
            renderFractal(xhr.response)
        }
    };
    var data = JSON.stringify([canvas.width, canvas.height]);
    console.time('fract plunder compute')
    console.time('fract total time')
    xhr.send(data);
  }

  function renderFractal(fractal) {
    console.timeEnd('fract plunder compute')
    console.time('fract js img render')
    const canvas = document.querySelector("canvas");
    const ctx = canvas.getContext("2d");

    var id = ctx.createImageData(canvas.width, canvas.height);

    var fractalBytes = new Uint8Array(fractal);
    var fractalWithAlpha = [];
    fractalBytes.forEach(function (v, i) {
      if (i != 0 && i % 3 == 0) { fractalWithAlpha.push(255); } // add alpha
      fractalWithAlpha.push(v);
    });
    fractalWithAlpha.push(255);

    id.data.set(fractalWithAlpha);

    ctx.putImageData( id, 0, 0 );
    console.timeEnd('fract js img render')
    console.timeEnd('fract total time')
  }

  function resize_canvas() {
    var width =
        document.querySelector("#canvas_width").value;
    var height =
        document.querySelector("#canvas_height").value;
    const canvas = document.querySelector("canvas");
    canvas.width = width;
    canvas.height = height;
  }

  document.getElementById('resize_btn').onclick = function(){resize_canvas()};

  resize_canvas();
})();

