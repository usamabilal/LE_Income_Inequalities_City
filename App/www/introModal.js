// Get Elements
navItems = document
  .querySelector("[data-tabsetid='3409']")
  .querySelectorAll('li');

  navItems.array.forEach(element => {
    element.classList.add("active")
  });
