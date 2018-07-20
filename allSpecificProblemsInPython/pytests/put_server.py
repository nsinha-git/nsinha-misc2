"""
Author: Nish Sinha
Licensed To: Peritus.ai

"""

# python -m SimpleHTTPPutServer 8080
import SimpleHTTPServer
import BaseHTTPServer
import random
import datetime

class SputHTTPRequestHandler(SimpleHTTPServer.SimpleHTTPRequestHandler):

    def do_WORK(self):
        print(self.headers)
        #length = int(self.headers["Content-Length"])
        length = 100

        path = self.translate_path(self.path) + "/rand_drop_" +  str(random.randint(1, 100000))
        with open(path, "wb") as dst:
            dst.write(self.rfile.read(length))

    def do_PUT(self):
        return self.do_WORK();

    def do_POST(self):
        return self.do_WORK();

    def do_POST(self):
        return self.do_WORK();

    def test(self):
        f = open("/tmp",'r')





if __name__ == '__main__':
    SimpleHTTPServer.test(HandlerClass=SputHTTPRequestHandler)

