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

const app = Main.embed(document.getElementById('root'));

var dogName, unitSystem, defaults;

if (dogName = localStorage.getItem('dogName')) {
    app.ports.gotDogName.send(dogName);
}

if (unitSystem = localStorage.getItem('unitSystem')) {
    app.ports.gotUnitSystem.send(unitSystem);
}

if (defaults = localStorage.getItem('defaults')) {
    app.ports.gotDefaults.send(JSON.parse(defaults));
}

dexieDb.events.toArray().then((events) => {
    if (events.length == 0) {
        console.log("No events to start with...");
    }
    events
        .map(parseEventForElmInterop)
        .forEach(e => app.ports.gotEventFromDatabase.send(e));
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
        app.ports.gotEventFromDatabase.send(parseEventForElmInterop(event));
    });
});

app.ports.saveDogName.subscribe((name) => {
    localStorage.setItem('dogName', name.toString());
});

app.ports.saveUnitSystem.subscribe((unitSystem) => {
    localStorage.setItem('unitSystem', unitSystem.toString());
});

app.ports.saveDefaults.subscribe((defaults) => {
    localStorage.setItem('defaults', JSON.stringify(defaults));
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