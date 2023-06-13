import { NextApiRequest } from 'next';
import { NextApiResponse } from 'next';
import { exec } from 'child_process';

export default async function myApiRoute(
    req: NextApiRequest,
    res: NextApiResponse
) {
    if (req.method !== 'POST') {
      res.status(405).send({ message: 'Only POST requests allowed' })
      return
    }
    const body = JSON.parse(JSON.stringify(req.body))
    var sender_addr = body["sender_addr"]
    var amount = body["amount"]
    var receiver_addr = body["receiver_addr"]
    var password = "password"
    const command = `python hello.py ${sender_addr} ${amount} ${receiver_addr} ${password}`; // Replace with your own command
    exec(command, (error, stdout, stderr) => {
      if (error) {
        console.error(`Error running command: ${error.message}`);
        res.status(500).send('Server Error');
        return;
      }
      if (stderr) {
        console.error(`Command stderr: ${stderr}`);
        res.status(500).send('Server Error');
        return;
      }
      console.log(`Command stdout: ${stdout}`);
      res.status(200).send({"response": stdout});
    });
  }
  