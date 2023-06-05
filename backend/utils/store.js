import { createTypedHooks } from 'easy-peasy';
import { action } from 'easy-peasy';
import { createStore, persist } from 'easy-peasy';
const model = {
    wallet: { connected: false, name: '', address: '' },
    setWallet: action((state, newWallet) => { state.wallet = newWallet; }),
    availableWallets: [],
    setAvailableWallets: action((state, newAvailableWallets) => { state.availableWallets = newAvailableWallets; }),
};
const store = createStore(persist(model));
export default store;
const { useStoreActions, useStoreState, useStoreDispatch, useStore } = createTypedHooks();
export { useStoreActions, useStoreState, useStoreDispatch, useStore };
