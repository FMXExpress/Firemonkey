from os import listdir, mkdir, path
import SimpleHTTPServer
import SocketServer
from time import sleep
from urllib import urlretrieve

# Server configuration
HOST = "localhost"
PORT = 8000

# Sample images.
script__dir = path.dirname(__file__)
data_dir = path.join(script__dir, "data")
if not path.exists(data_dir):
	mkdir(data_dir)
if not listdir(data_dir):
	print "Downloading sample images"
	lolcat_file_names = [
		"aMlUpJB.jpg", "fmXnXWn.png", "IWSnWNt.jpg", "QgA69dC.png", "SpCbHBI.jpg"]
	for file_name in lolcat_file_names:
		url = "http://i.imgur.com/" + file_name
		local_path = path.join(data_dir, file_name)
		urlretrieve(url, local_path)

# Server initialization
Handler = SimpleHTTPServer.SimpleHTTPRequestHandler
httpd = SocketServer.TCPServer((HOST, PORT), Handler)
print "Drowsy server at %s:%d"%(HOST,PORT)
while True:
    httpd.handle_request()
    sleep(0.250)
