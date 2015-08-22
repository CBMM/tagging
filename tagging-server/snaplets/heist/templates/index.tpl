<apply template="base">

  <ifLoggedIn>
    <p>Hi. You're logged in as <loggedInUser/>.</p>
    <p><a href="api/logout">Logout</a></p>
  </ifLoggedIn>

  <ifLoggedOut>
    <apply template="_login"/>
  </ifLoggedOut>

</apply>
