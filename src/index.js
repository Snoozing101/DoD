import './main.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';
import PouchDB from 'pouchdb'

const db = new PouchDB('doddb')

const app = Elm.Main.init({
  node: document.getElementById('root')
});

app.ports.saveCharacterToDB.subscribe(message => {
    console.log('Port emitted a new message: ' + message);
});

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
