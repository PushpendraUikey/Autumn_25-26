#include <iostream>
#include <cstring>
#include <unistd.h>        // for close()
#include <arpa/inet.h>     // for socket functions

#define PORT 65432

int main() {
    int server_fd, new_socket;
    struct sockaddr_in address;
    char buffer[1024] = {0};
    int opt = 1;
    int addrlen = sizeof(address);

    // Create socket
    if ((server_fd = socket(AF_INET, SOCK_STREAM, 0)) == 0) {
        perror("Socket failed");
        return 1;
    }

    // Set socket options (reuse address)
    setsockopt(server_fd, SOL_SOCKET, SO_REUSEADDR | SO_REUSEPORT, &opt, sizeof(opt));

    // Bind to port
    address.sin_family = AF_INET;
    address.sin_addr.s_addr = INADDR_ANY;   // receive from any IP
    address.sin_port = htons(PORT);

    if (bind(server_fd, (struct sockaddr *)&address, sizeof(address)) < 0) {
        perror("Bind failed");
        return 1;
    }

    // Listen for incoming connection
    if (listen(server_fd, 1) < 0) {
        perror("Listen failed");
        return 1;
    }

    std::cout << "Waiting for connection..." << std::endl;

    // Accept connection
    if ((new_socket = accept(server_fd, (struct sockaddr *)&address, 
                             (socklen_t*)&addrlen)) < 0) {
        perror("Accept failed");
        return 1;
    }

    std::cout << "Connected to sender!" << std::endl;

    // Receive message character-by-character
    while (true) {
        int valread = recv(new_socket, buffer, sizeof(buffer) - 1, 0);
        if (valread <= 0) break;  // connection closed
        buffer[valread] = '\0';
        std::cout << buffer;      // print received chunk
        std::cout.flush();
    }

    std::cout << "\nConnection closed." << std::endl;

    close(new_socket);
    close(server_fd);
    return 0;

    
}
