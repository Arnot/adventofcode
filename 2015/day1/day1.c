#include <stdio.h>

int main(int argc, char** argv)
{
        int floor = 0;
        char current;
        int position = 0;
        int first_negative = 1;

        while (scanf("%1c", &current) == 1){
                position++;

                if (current == '(')
                        floor++;
                else if (current == ')')
                        floor--;
                else // other character found
                        break;

                if (floor < 0 && first_negative) {
                        printf("Went to floor -1 for the first time on position %d.\n", position);
                        first_negative = 0;
                }
        }

        printf("Final floor: %d\n", floor);

        return 0;
}
