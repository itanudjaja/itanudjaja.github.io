document.querySelectorAll(".deck").forEach(function (deck) {
  var slides = Array.prototype.slice.call(deck.querySelectorAll("img"));
  if (!slides.length) return;

  slides.forEach(function (img) {
    img.classList.add("slide");
  });

  var stage = document.createElement("div");
  stage.className = "stage";

  var prev = document.createElement("button");
  prev.type = "button";
  prev.className = "deck-arrow prev";
  prev.setAttribute("aria-label", "Previous slide");
  prev.textContent = "\u2039";

  var next = document.createElement("button");
  next.type = "button";
  next.className = "deck-arrow next";
  next.setAttribute("aria-label", "Next slide");
  next.textContent = "\u203a";

  var thumbs = document.createElement("div");
  thumbs.className = "thumbs";

  slides.forEach(function (img, i) {
    stage.appendChild(img);
    var thumb = img.cloneNode();
    thumb.removeAttribute("class");
    thumb.addEventListener("click", function () {
      show(i);
    });
    thumbs.appendChild(thumb);
  });

  stage.insertBefore(prev, stage.firstChild);
  stage.appendChild(next);
  deck.textContent = "";
  deck.appendChild(stage);
  deck.appendChild(thumbs);

  var index = 0;

  function show(i) {
    index = (i + slides.length) % slides.length;
    slides.forEach(function (img, n) {
      img.classList.toggle("active", n === index);
    });
    Array.prototype.forEach.call(thumbs.children, function (thumb, n) {
      thumb.classList.toggle("active", n === index);
    });
    var active = thumbs.children[index];
    if (active) {
      var left = active.offsetLeft - (thumbs.clientWidth - active.clientWidth) / 2;
      thumbs.scrollTo({ left: left, behavior: "smooth" });
    }
  }

  prev.addEventListener("click", function () {
    show(index - 1);
  });
  next.addEventListener("click", function () {
    show(index + 1);
  });

  deck.tabIndex = 0;
  deck.addEventListener("keydown", function (event) {
    if (event.key === "ArrowLeft") {
      show(index - 1);
    } else if (event.key === "ArrowRight") {
      show(index + 1);
    }
  });

  deck.classList.add("is-ready");
  show(0);
});
