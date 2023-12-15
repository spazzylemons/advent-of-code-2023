#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct Lens {
    char label[8];
    int value;
    struct Lens *next;
} Lens;

// Instead of freeing lenses when removed, add them to this stack to minimize
// memory allocations
static Lens *allocstack = NULL;

static Lens *remove_from_box(Lens *lens, const char *label) {
    Lens *start = lens;
    Lens *previous = NULL;

    while (lens != NULL) {
        if (!strcmp(lens->label, label)) {
            if (previous != NULL) {
                previous->next = lens->next;
            } else {
                start = lens->next;
            }

            lens->next = allocstack;
            allocstack = lens;

            return start;
        } else {
            previous = lens;
            lens = lens->next;
        }
    }

    return start;
}

static Lens *add_to_box(Lens *lens, const char *label, int value) {
    Lens *start = lens;
    Lens *previous = NULL;

    while (lens != NULL) {
        if (!strcmp(lens->label, label)) {
            lens->value = value;
            return start;
        } else {
            previous = lens;
            lens = lens->next;
        }
    }

    if (allocstack == NULL) {
        lens = malloc(sizeof(Lens));
    } else {
        lens = allocstack;
        allocstack = allocstack->next;
    }

    strcpy(lens->label, label);
    lens->value = value;
    lens->next = NULL;

    if (previous != NULL) {
        previous->next = lens;
    } else {
        start = lens;
    }

    return start;
}

static int get_hash(const char *label) {
    int result = 0;
    while (*label) {
        result += *label++;
        result = (result * 17) & 255;
    }
    return result;
}

static Lens *boxes[256];

int main(void) {
    FILE *file = fopen("input", "r");
    int part1 = 0;
    int part2 = 0;

    char label[9];
    int labelidx = 0;
    for (;;) {
        char c = fgetc(file);
        if (c == ',' || c == '\n') {
            label[labelidx] = '\0';
            part1 += get_hash(label);

            if (label[labelidx - 1] == '-') {
                label[labelidx - 1] = '\0';
                int hash = get_hash(label);
                boxes[hash] = remove_from_box(boxes[hash], label);
            } else {
                int value = label[labelidx - 1] - 0x30;
                label[labelidx - 2] = '\0';
                int hash = get_hash(label);
                boxes[hash] = add_to_box(boxes[hash], label, value);
            }

            if (c == '\n') {
                break;
            }

            labelidx = 0;
        } else {
            label[labelidx++] = c;
        }
    }
    fclose(file);

    printf("%d\n", part1);

    for (int i = 0; i < 256; i++) {
        Lens *lens = boxes[i];
        int j = 1;
        while (lens != NULL) {
            Lens *next = lens->next;
            part2 += (i + 1) * j * lens->value;
            free(lens);
            j++;
            lens = next;
        }
    }

    while (allocstack != NULL) {
        Lens *next = allocstack->next;
        free(allocstack);
        allocstack = next;
    }

    printf("%d\n", part2);

    return 0;
}
