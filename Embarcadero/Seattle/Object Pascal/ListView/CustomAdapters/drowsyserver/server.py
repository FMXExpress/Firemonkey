import SimpleHTTPServer
import SocketServer
from time import sleep

HOST = "10.130.1.71"
PORT = 8000

Handler = SimpleHTTPServer.SimpleHTTPRequestHandler

httpd = SocketServer.TCPServer((HOST, PORT), Handler)

print "Drowsy server at %s:%d"%(HOST,PORT)

while True:
    httpd.handle_request()
    sleep(0.250)
