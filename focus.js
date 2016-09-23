
    function shinyjs.Check () {
      if (document.Testform.Todesjahr.value != "1832") {
        alert("Sie wissen es einfach nicht!");
        document.Testform.Todesjahr.focus();
      } else {
        alert("Bravo!");
        window.location.href = "erfolg.htm";
      }
    }

