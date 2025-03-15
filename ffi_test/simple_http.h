#pragma once
#include <stddef.h>

int http_init_server(int port);
int http_accept_client(int server_handle);
const char* http_read_request(int client_handle);
const char* http_get_path(const char* request);
void http_send_response(int client_handle, const char* html_content);
void http_close_client(int client_handle);
void http_shutdown(int server_handle);
