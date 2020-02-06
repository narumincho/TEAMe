import { Elm } from "../main/source/Main.elm";
import * as firebase from "firebase/app";
import "firebase/firestore";

const app = Elm.Main.init({ flags: null });

app.ports.setText.subscribe(e => {
  requestAnimationFrame(() => {
    (document.getElementById(e.id) as
      | HTMLInputElement
      | HTMLTextAreaElement).value = e.text;
  });
});
