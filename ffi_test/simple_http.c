#include "simple_http.h"
#include <netinet/in.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <unistd.h>

#define BUFFER_SIZE 4096

int http_init_server(int port)
{
    int server_fd;
    struct sockaddr_in address;
    if ((server_fd = socket(AF_INET, SOCK_STREAM, 0)) == 0) {
        perror("socket failed");
        return -1;
    }
    int opt = 1;
    if (setsockopt(server_fd, SOL_SOCKET, SO_REUSEADDR, &opt, sizeof(opt))) {
        perror("setsockopt failed");
        close(server_fd);
        return -1;
    }
    address.sin_family = AF_INET;
    address.sin_addr.s_addr = INADDR_ANY;
    address.sin_port = htons(port);
    if (bind(server_fd, (struct sockaddr*)&address, sizeof(address)) < 0) {
        perror("bind failed");
        close(server_fd);
        return -1;
    }
    if (listen(server_fd, 10) < 0) {
        perror("listen failed");
        close(server_fd);
        return -1;
    }

    return server_fd;
}

int http_accept_client(int server_handle)
{
    struct sockaddr_in address;
    int addrlen = sizeof(address);

    int client_socket = accept(server_handle, (struct sockaddr*)&address, (socklen_t*)&addrlen);
    if (client_socket < 0) {
        perror("accept failed");
        return -1;
    }

    return client_socket;
}

const char* http_read_request(int client_handle)
{
    static char buffer[BUFFER_SIZE];
    memset(buffer, 0, BUFFER_SIZE);

    ssize_t valread = read(client_handle, buffer, BUFFER_SIZE - 1);
    if (valread < 0) {
        perror("read failed");
        buffer[0] = '\0';
    } else {
        buffer[valread] = '\0';
    }

    return buffer;
}

const char* http_get_path(const char* request)
{
    static char path[BUFFER_SIZE];
    memset(path, 0, BUFFER_SIZE);
    if (strncmp(request, "GET ", 4) == 0) {
        const char* start = request + 4;
        const char* end = strstr(start, " HTTP");

        if (end && end > start) {
            size_t pathLen = end - start;
            if (pathLen < BUFFER_SIZE) {
                strncpy(path, start, pathLen);
                path[pathLen] = '\0';
                return path;
            }
        }
    }

    strcpy(path, "/");
    return path;
}

void http_send_response(int client_handle, const char* content)
{
    char response[BUFFER_SIZE];
    snprintf(response, BUFFER_SIZE,
        "HTTP/1.1 200 OK\r\n"
        "Content-Type: text/html\r\n"
        "Connection: close\r\n"
        "\r\n%s",
        content);

    ssize_t sent = write(client_handle, response, strlen(response));
    if (sent < 0) {
        perror("send failed");
    }
}

void http_close_client(int client_handle)
{
    close(client_handle);
}

void http_shutdown(int server_handle)
{
    close(server_handle);
}
