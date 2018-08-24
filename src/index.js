import './main.css';
import { Main } from './Main.elm';
import Dexie from 'dexie';
import registerServiceWorker from './registerServiceWorker';

if (!window.indexedDB) {
    window.alert("Your browser doesn't support a stable version of IndexedDB.  Long-term storage will not be enabled.  Please try a different browser.");
}

const dexieDb = new Dexie('Dev-DogDoDb');
dexieDb.version(2).stores({
    events: '++id, eventType, itemType, itemName, quantity, timestamp'
});

dexieDb.events.toArray().then((events) => {
    if (events.length == 0) {
        console.log("No events to start with...");
    }
    events
        .map(parseEventForElmInterop)
        .forEach(e => app.ports.gotEventFromDatabase.send(e));
});

const app = Main.embed(document.getElementById('root'));

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
        app.ports.gotEventFromDatabase.send(parseEventForElmInterop(event));
    });
});

registerServiceWorker();

const parseEventForDbStorage = (event) => {
    var newEvent = event;
    var parsedDate = Date.parse(event.timestamp);
    newEvent.timestamp = parsedDate;
    return newEvent;
};

const parseEventForElmInterop = (event) => {
    var newEvent = event;
    var isoDate = new Date(event.timestamp).toISOString();
    newEvent.timestamp = isoDate;
    return newEvent;
};