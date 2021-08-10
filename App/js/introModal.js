function makeItemsActive() {
  navItems = document.
    querySelector('.navbar-nav').
    querySelectorAll('li');

  navItems.array.forEach((element) => {
    element.classList.add('active');
  });
}
  navItems.forEach((element) => {
    element.classList.remove('active');
  });

  navItems;[1]