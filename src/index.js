import './main.css';
import { Main } from './Main.elm';
import Dexie from 'dexie';
import registerServiceWorker from './registerServiceWorker';

registerServiceWorker();

if (!window.indexedDB) {
    window.alert("Your browser doesn't support a stable version of IndexedDB.  Long-term storage will not be enabled.  Please try a different browser.");
}

const dexieDb = new Dexie('DogDoDb');

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
            .forEach(e => app.ports.retrievedNewEvent.send(e));
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

    let newEvent = Object.assign({}, event);
    newEvent.timestamp = Date.parse(event.timestamp);
    delete newEvent.id;

    dexieDb.events.add(newEvent).then((id) => {
        app.ports.retrievedNewEvent.send(parseEventForElmInterop(event, id));
    });
});

app.ports.saveSettings.subscribe((settings) => {
    localStorage.setItem('settings', JSON.stringify(settings));
    app.ports.retrievedSettings.send(settings);
})

app.ports.saveDefaults.subscribe((defaults) => {
    localStorage.setItem('defaults', JSON.stringify(defaults));
});

app.ports.updateEvent.subscribe((event) => {
    if (!window.indexedDB) {
        console.log("IndexedDB Unavailable")
        return;
    }

    if (!dexieDb) {
        console.log("DB access not granted");
        return;
    }

    let newEvent = Object.assign({}, event);
    newEvent.timestamp = Date.parse(event.timestamp);

    dexieDb.events.update(event.id, newEvent).then((updated) => {
        if (updated) {
            app.ports.retrievedEventUpdate.send(parseEventForElmInterop(newEvent));
        }
    });
});

const parseEventForElmInterop = (event, id) => {
    var newEvent = Object.assign({}, event);
    var isoDate = new Date(event.timestamp).toISOString();
    newEvent.timestamp = isoDate;
    newEvent.id = newEvent.id || id;
    return newEvent;
};