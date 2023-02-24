const Web3 = require('web3');
require('dotenv').config();
// Replace with your own Web3 provider endpoint
const web3 = new Web3(process.env.ENDPOINT);

// Replace with the address of the contract you want to interact with
const contractAddress = '0x225a400be34eAB0A0E54862eB29a422e17995Cd7';

// Replace with the function signature you want to call
const functionSignature = process.argv[2];

// Replace with the argument you want to pass to the function
const arg = process.argv[3];
const paddedArg = arg.padStart(64, '0');

const pk = process.env.PRIVATE_KEY;

// Create the raw transaction object
const txObject = {
  from: process.env.ADDRESS,
  to: contractAddress,
  gas: 300000,
  data: functionSignature + paddedArg,
};

// Sign the transaction and send it to the blockchain
web3.eth.accounts.signTransaction(txObject, pk)
    .then((signedTx) => {
        web3.eth.sendSignedTransaction(signedTx.rawTransaction)
            .on('receipt', console.log)
            .on('error', console.error);
    });

//print padded argument
console.log(paddedArg);