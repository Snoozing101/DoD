import './main.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';
import PouchDB from 'pouchdb'

const db = new PouchDB('doddb')

const app = Elm.Main.init({
  node: document.getElementById('root')
});

app.ports.saveCharacterToDB.subscribe(message => {
    // console.log('Port emitted a new message: ' + message);
    var characterToSave = JSON.parse(message);
    db.put(characterToSave);
});


app.ports.retrieveCharacterList.subscribe(function() {
    db.allDocs({
        include_docs : true
    }).then(function (result) {
        var allRows = result.rows;
        var charList = allRows.map(x => x['id'] + ' - ' + x['doc']['Class']);
        app.ports.characterListReceiver.send(charList);
    }).catch(function (err) {
        console.log(err);
    });
});

app.ports.retrieveCharacter.subscribe(character => {
    console.log(character);
    db.get(character).then(function (doc) {
        console.log(doc);
        app.ports.characterReceiver.send(JSON.stringify(doc));
    }).catch(function (err) {
        console.log(err);
    });
});

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
