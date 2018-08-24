import './main.css';
import { Main } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

const app = Main.embed(document.getElementById('root'));

app.ports.storeAdministerEvent.subscribe((event) => {
    console.log(event);
});

app.ports.storePurchaseEvent.subscribe((event) => {
    console.log(event);
});

registerServiceWorker();
