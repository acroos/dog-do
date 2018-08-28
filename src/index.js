import './main.css';
import { Main } from './Main.elm';
import Dexie from 'dexie';
import registerServiceWorker from './registerServiceWorker';

registerServiceWorker();

if (!window.indexedDB) {
    window.alert("Your browser doesn't support a stable version of IndexedDB.  Long-term storage will not be enabled.  Please try a different browser.");
}

const dexieDb = new Dexie('Dev-DogDoDb');

dexieDb.version(2).stores({
    events: '++id, eventType, itemType, itemName, quantity, timestamp'
});

const app = Main.embed(document.getElementById('root'));

var defaults, settings;
if (settings = localStorage.getItem('settings')) {
    app.ports.retrievedSettings.send(JSON.parse(settings));
}

if (defaults = localStorage.getItem('defaults')) {
    app.ports.retrievedDefaults.send(JSON.parse(defaults));
}

dexieDb.events.orderBy('timestamp').toArray().then((events) => {
        if (events.length == 0) {
            console.log("No events to start with...");
        }
        events
            .map(parseEventForElmInterop)
            .forEach(e => app.ports.retrievedEventFromDatabase.send(e));
});

app.ports.saveEvent.subscribe((event) => {
    if (!window.indexedDB) {
        console.log("IndexedDB Unavailable")
        return;
    }

    if (!dexieDb) {
        console.log("DB access not granted");
        return;
    }

    var parsedEvent = parseEventForDbStorage(event);
    dexieDb.events.add(parsedEvent).then((id) => {
        app.ports.retrievedEventFromDatabase.send(parseEventForElmInterop(event));
    });
});

app.ports.saveSettings.subscribe((settings) => {
    localStorage.setItem('settings', JSON.stringify(settings));
    app.ports.retrievedSettings.send(settings);
})

app.ports.saveDefaults.subscribe((defaults) => {
    localStorage.setItem('defaults', JSON.stringify(defaults));
});

const parseEventForDbStorage = (event) => {
    var newEvent = Object.assign({}, event);
    var parsedDate = Date.parse(event.timestamp);
    newEvent.timestamp = parsedDate;
    return newEvent;
};

const parseEventForElmInterop = (event) => {
    var newEvent = Object.assign({}, event);
    var isoDate = new Date(event.timestamp).toISOString();
    newEvent.timestamp = isoDate;
    return newEvent;
};