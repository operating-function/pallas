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
    xhr.onreadystatechange = function () {
        if (xhr.readyState === 4 && xhr.status === 200) {
            var json = JSON.parse(xhr.responseText);
            renderFractal(json)
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
    id.data.set(fractal);

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

  document.getElementById('resize_btn').onclick = resize_canvas();

  resize_canvas();
})();

