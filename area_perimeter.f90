! Program to find the area and perimeter of a rectangle
program area_perimeter
    implicit none
    integer :: a, b, area, perimeter

    print *, "Enter the length and breadth of the rectangle:"
    read *, a, b

    area = a * b; perimeter = 2 * (a + b)
    print *, "Area =", area, ", Perimeter =", perimeter
end program area_perimeter
