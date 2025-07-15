## Prerequisites

- Linux machine to act as remote server, preferably Ubuntu based systems. Although this server can run on any Linux machine, but right now the PVS installation script in only compatible with ubuntu. If you already have PVS installed on your server you can just use the '-pvspath' flag, this will work with any Linux system.
- rsync installed on the Linux machine.

## Installation and Setup

1. Clone this repository.
2. Change to the main working directory and install dependencies:<br>
<code>cd {directory}
npm install
npm run build</code><br>


## Running the Server

Run the server using the following command: <code>npm run start</code>
### Options

The following arguments are optional. <b>If not provided, default values will be used</b>:

- `--dataPath`: Path to store server data (default: '/tmp/pvs-sync-server').
- `--port`: Port for the WebSocket server (default: 8080).
- `--pvsPath`: Full path to the PVS8 installation directory (default: "{homedir}/pvs8/PVS"). This path should contain the pvs executable that starts the pvs server. If pvs executable not accesible/ no path given , then the server will attempt to run a clean install of pvs8 on given path. At start if no path is provided and pvs8 hasn't been installed on the system then the <b>server will automatically install pvs8 at given path</b>.

Sometimes the flag identifier "--" needs to be specified between the command and the arguments. In that case, try the following:
<code>npm start "--" [Options]</code>

## Configuring PVS VSCode Extension

To use this server with the PVS VSCode extension:

1. Set the "Remote Active" flag to true.
2. Set the "Remote Port" to the port where this server is running.
3. Configure the Linux server's IP, hostname, and the path to the private key on your local machine for SSH access.

## Running server via executable binary
You can also run this server on a linux machine without having node installed. Clone this repository and do "npm install" along with "npm run build". Then you can run "npm run build-exe" if you have "pkg" package installed, if not run "npm install -g pkg" first. This will build a binary executable which you can run with same flags as nodejs script.

