<html>

  <head>
    <script src="https://code.jquery.com/jquery-3.0.0.min.js"></script>
    <link href='https://fonts.googleapis.com/css?family=Lobster' rel='stylesheet' type='text/css'>
    <style>

      .content{
        // background-color: hsla(0,0%,90%,1);
        height: 100%;
      display: flex;
      align-items: center;
      justify-content: space-around;
      background: repeating-radial-gradient(
        circle, #FFC65F , #FFC65F 40px, #FFD280 40px, #ffd280 80px
      )
      }

      .linkbox{
      width: 80%;
      background-color: white;
      border-radius: 0px;
      box-shadow: 5px 5px 5px rgba(0,0,0,0.1);
      display:flex
      flex-direction: column;
      }

      .top-half{
      display: flex;
      flex-direction: row-nowrap;
      align-items: center;
      }

      .bottom-half{
      display: flex;
      justify-content: space-around;
      align-items: center;
      }

      hr {
      width: 80%;
      background-color: blue;
      }

      .imgdiv, .imgdiv img {
         max-width: 100px;
      }

      .info {
      padding: 20px;
      display: flex;
      justify-content: space-around;
      flex-grow:2;
      font-size: 24pt;
      font-family: 'Lobster', cursive;
      }

      .button, input.button{
      border: 1px solid orange;
      margin: 10px;
      padding: 10px;
      white-space: nowrap;
      box-shadow: 5px 5px 5px rgba(0,0,0,0.1);
      text-decoration: none;
      color: orange;
      font-weight: bolder;
      max-width:200px;
      text-align: center;
      }

      .button span, input.button{
      font-size: 14pt;
      font-weight: bolder;
      }


      a {
      text-decoration: none;
      color: orange;
      font-weight: bolder;
      }

      form { margin-bottom: 0px; }
      input { background-color: white; font-weight: bolder; }

    </style>

    <script>
      function checkprogress(){
        $.ajax('/api/progress',
               {success: function(d, t, x) {
                           console.log(d);
                           console.log(t);
                           handleProgress(d);
                         },
                error: function(x, t, e) {
                         alert('Error');
                         console.warn(x);
                         console.warn(t);
                       },
               });
      }

      function progressText(p){
        var tx;
        var nDone = p['_progressNResponses'];
        var nTotal = p['_progressNTotal'];
        if (nDone == 0) {
          tx = nTotal + ' trials';
        } else {
          tx = nDone + ' / ' + nTotal;
        }
        return tx;
      }

      function handleProgress(p){
        var progressNumber = $('#progressNumber');
        var submitButton = $('#submitButton');
        progressNumber.text( progressText(p) );

        if (nDone >= nTotal) {
          submitButton.disabled = false;
          submitButton.hidden = false;
        } else {
          submitButton.disabled = true;
          submitButton.hidden = true;
        }
      }

    </script>

  </head>
  <body>

    <div class="content">

      <div class="linkbox">
        <div class="top-half">

          <div class="imgdiv">
            <img src="https://raw.githubusercontent.com/CBMM/tagging/master/app/static/media/img/camera.png"/>
          </div>

          <div class="info">
            <span class="title">
              Start labeling movie actors
            </span>

            <span class="progressNumber" id="progressNumber">
            </span>

          </div>

          <a href="${turklink}" target="_blank">
            <div class="button">
              <span>Click here &#9658;</span>
            </div>
          </a>

        </div>
        <hr/>
        <div class="bottom-half">
          <div class="button" onclick="checkprogress()">
            <span>Check progress</span>
          </div>
          <div id="submission-form" hidden disabled>
            <form action="${finishurl}" method="post">
              <input type="submit" id="submitButton" value="Submit" class="button"/>
            </form>
          </div>
        </div>

      </div>
    </div>
  </body>

</html>
