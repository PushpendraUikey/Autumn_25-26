import socket
import time

HOST = "172.28.255.156"   # receiver IP
PORT = 65432

def send_bits(msg):
    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
        s.connect((HOST, PORT))
        print("Connected to receiver...")
        for m in msg:
            s.sendall(m.encode())   # convert char -> bytes
            # time.sleep(0.5)         # delay between chars
        print("Message sent!")

if __name__ == "__main__":
    msg = "Hello World"
    send_bits(msg)
