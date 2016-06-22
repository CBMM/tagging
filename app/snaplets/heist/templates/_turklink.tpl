<html>

  <head>
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
      maxheight: 200px;
      width: 80%;
      background-color: white;
      border-radius: 0px;
      box-shadow: 5px 5px 5px rgba(0,0,0,0.1);
      display: flex;
      flex-direction: row-nowrap;
      align-items: center;
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

      .button{
      border: 1px solid orange;
      margin: 10px;
      padding: 10px;
      white-space: nowrap;
      box-shadow: 5px 5px 5px rgba(0,0,0,0.1);
      }

      a {
      text-decoration: none;
      color: orange;
      }

    </style>
  </head>
  <body>

    <div class="content">

      <div class="linkbox">

        <div class="imgdiv">
          <img src="file:///home/greghale/Programming/tagging/app/static/media/img/camera.png"/>
        </div>

        <div class="info">
          Start labeling movie actors
        </div>

        <a href="${turklink}" target="_blank">
        <div class="button">
          Click here &#9658;
        </div>
        </a>

      </div>

    </div>
    
  </body>

</html>
