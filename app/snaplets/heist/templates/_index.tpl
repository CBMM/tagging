<apply template="base">

  <ifLoggedIn>
    <p>Hi. You're logged in as <loggedInUser/>.</p>

    <h3>Your assignments:</h3>

    <ul>
    <assignments>
      <li><a href="${link}"> <assignmentLabel/> </a> </li> 
    </assignments>
    </ul>

    <p><a href="/logout">Logout</a></p>
  </ifLoggedIn>

  <ifLoggedOut>
    <apply template="_login"/>
  </ifLoggedOut>

</apply>
