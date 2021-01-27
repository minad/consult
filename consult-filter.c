#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/select.h>
#include <sys/wait.h>
#include <unistd.h>

static void chk(const char* str, int err) {
    if (err) {
        perror(str);
        exit(1);
    }
}

static void wait_process(pid_t* pid, int* fd) {
    close(*fd);
    chk("waitpid failed", waitpid(*pid, 0, 0) < 0);
    *pid = *fd = -1;
}

static void start_source(pid_t* pid, int* fd, const char* cmd) {
    int p[2];
    chk("pipe failed", pipe(p) < 0);
    chk("fork failed", (*pid = fork()) < 0);
    if (!*pid) {
        chk("dup2 failed", dup2(p[1], STDOUT_FILENO) < 0);
        close(p[0]);
        execl("/bin/sh", "sh", "-c", cmd, 0);
        chk("execl failed", 1);
    }
    close(p[1]);
    chk("fcntl failed", fcntl(p[0], F_SETFL, O_NONBLOCK) < 0);
    *fd = p[0];
}

static void end_filter(pid_t* pid, int* fd) {
    wait_process(pid, fd);
    chk("write failed", write(STDOUT_FILENO, "\3", 1) != 1); // ETX
}

static void start_filter(pid_t* pid, int* fd, const char* cmd, const char* filter) {
    if (*pid >= 0) {
        chk("kill failed", kill(*pid, SIGKILL) < 0);
        end_filter(pid, fd);
    }
    int p[2];
    chk("pipe failed", pipe(p) < 0);
    chk("fork failed", (*pid = fork()) < 0);
    if (!*pid) {
        chk("dup2 failed", dup2(p[0], STDIN_FILENO));
        close(p[1]);
        execl("/bin/sh", "sh", "-c", cmd, filter, 0);
        chk("execl failed", 1);
    }
    close(p[0]);
    chk("fcntl failed", fcntl(p[1], F_SETFL, O_NONBLOCK) < 0);
    chk("write failed", write(STDOUT_FILENO, "\2", 1) != 1); // STX
    *fd = p[1];
}

int main(int argc, char* argv[]) {
    pid_t filter_pid = -1, source_pid = -1;
    int filter_fd = -1, source_fd = -1;
    char* data = 0;
    size_t data_size = 0, data_written = 0, data_avail = 0;

    if (argc != 3) {
        fprintf(stderr, "Usage: %s <source> <filter>\n", argv[0]);
        exit(1);
    }

    // Block pipe signal since filter may terminate early
    sigset_t set;
    sigemptyset(&set);
    sigaddset(&set, SIGPIPE);
    chk("sigprocmask failed", sigprocmask(SIG_BLOCK, &set, 0) < 0);

    start_source(&source_pid, &source_fd, argv[1]);

    for (;;) {
        int max_fd = STDIN_FILENO;
        fd_set read_fds, write_fds;
        FD_ZERO(&read_fds);
        FD_SET(STDIN_FILENO, &read_fds);
        if (source_fd >= 0) {
            if (source_fd > max_fd)
                max_fd = source_fd;
            FD_SET(source_fd, &read_fds);
        }

        FD_ZERO(&write_fds);
        if (filter_fd >= 0 && data_written < data_size) {
            if (filter_fd > max_fd)
                max_fd = filter_fd;
            FD_SET(filter_fd, &write_fds);
        }

        chk("select failed", select(max_fd + 1, &read_fds, &write_fds, 0, 0) < 0);

        // Read new filter string
        if (FD_ISSET(STDIN_FILENO, &read_fds)) {
            char buf[1024];
            ssize_t len = read(STDIN_FILENO, buf, sizeof (buf));
            chk("read from stdin failed", len < 0);
            if (!len)
                break;
            if (buf[len - 1] != '\n') {
                fputs("invalid input\n", stderr);
                exit(1);
            }
            if (len > 1) {
                buf[len - 1] = 0;
                start_filter(&filter_pid, &filter_fd, argv[2], buf);
                data_written = 0;
            }
        }

        // Read new data from source
        if (source_fd >= 0 && FD_ISSET(source_fd, &read_fds)) {
            if (data_size + 0x100000 > data_avail) {
                data_avail = data_avail ? 2 * data_avail : 0x100000;
                chk("realloc failed", !(data = realloc(data, data_avail)));
            }
            ssize_t len = read(source_fd, data + data_size, data_avail - data_size);
            if (len < 0)
                chk("read from stdin failed", errno != EAGAIN);
            else if (len)
                data_size += len;
            else
                wait_process(&source_pid, &source_fd);
        }

        // Write new data to filter
        if (filter_fd >= 0 && FD_ISSET(filter_fd, &write_fds) && data_written < data_size) {
            ssize_t len = write(filter_fd, data + data_written, data_size - data_written);
            if (len < 0) {
                if (errno == EPIPE)
                    end_filter(&filter_pid, &filter_fd);
                else
                    chk("write failed", errno != EAGAIN);
            } else {
                data_written += len;
            }
        }

        // Source is done, wait for filter
        if (source_fd < 0 && filter_fd >= 0 && data_written == data_size)
            end_filter(&filter_pid, &filter_fd);
    }

    return 0;
}
