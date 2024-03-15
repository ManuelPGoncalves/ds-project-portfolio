from math import sqrt
import matplotlib.pyplot as plt
import matplotlib.patches as patches
from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg
import os
import platform
import tkinter as tk
from tkinter import simpledialog, messagebox

class Point:
    def __init__(self, x, y):
        self.x = x
        self.y = y

    def falls_in_rectangle(self, rectangle):
        # Check if point falls within the rectangle bounds
        if rectangle.lowleft[0] < self.x < rectangle.upright[0] and rectangle.lowleft[1] < self.y < rectangle.upright[
            1]:
            return True
        else:
            return False

    def distance_from_point(self, point):
        return sqrt((self.x - point.x) ** 2 + (self.y - point.y) ** 2)

class Rectangle:
    def __init__(self, lowleft, upright):
        self.lowleft = lowleft
        self.upright = upright

class GeometryGameGUI:
    """
            Class that initiates and controls the Geometry Game, a simple interactive game where
            the user inputs coordinates to define a rectangle and a point, and then
            the game determines whether the point lies inside the rectangle.

            The game consists of the following steps:
            1. Welcome message.
            2. User is prompted to enter the bottom-left and top-right coordinates of a rectangle,
               each entered as a pair of integers separated by a comma (e.g., 'x,y').
            3. User is prompted to enter the coordinates of a point in a similar manner.
            4. The game creates Rectangle and Point objects based on the user's input.
            5. It checks if the point lies within the boundaries of the rectangle.
            6. A visual representation of the rectangle and point is displayed using matplotlib,
               with the rectangle outlined in red and the point marked in blue.
            7. The result (True if the point is inside the rectangle, False otherwise) is printed.
            8. The user is asked if they wish to play again. If 'y' is selected, the game restarts;
               otherwise, the game ends.

            The game employs basic concepts of coordinate geometry and object-oriented programming
            to create an engaging educational tool. It demonstrates the use of classes, objects,
            conditionals, loops, and basic data visualization.
            """
    def __init__(self, master):
        self.master = master
        master.title("Geometry Game")

        # Create UI elements
        self.label = tk.Label(master, text="Welcome to the Geometry Game!")
        self.label.pack()

        self.play_button = tk.Button(master, text="Play", command=self.play)
        self.play_button.pack()

        self.result_label = tk.Label(master, text="")
        self.result_label.pack()

    def play(self):
        # Clear previous results
        self.result_label.config(text="")

        # Get rectangle coordinates
        llx, lly = map(int, simpledialog.askstring("Input",
                                                   "Enter the bottom-left coordinates of the rectangle separated by a comma:",
                                                   parent=self.master).split(','))
        urx, ury = map(int, simpledialog.askstring("Input",
                                                   "Enter the top-right coordinates of the rectangle separated by a comma:",
                                                   parent=self.master).split(','))

        # Get point coordinates
        px, py = map(int, simpledialog.askstring("Input", "Enter the point coordinates separated by a comma:",
                                                 parent=self.master).split(','))

        # Create objects
        rectangle = Rectangle((llx, lly), (urx, ury))
        point = Point(px, py)

        # Check if the point falls in the rectangle
        result = point.falls_in_rectangle(rectangle)

        # Visualization
        fig, ax = plt.subplots()
        rect = patches.Rectangle(rectangle.lowleft, rectangle.upright[0] - rectangle.lowleft[0],
                                 rectangle.upright[1] - rectangle.lowleft[1], linewidth=1, edgecolor='r',
                                 facecolor='none')
        ax.add_patch(rect)
        plt.plot(point.x, point.y, 'bo')
        plt.xlim(min(llx, px) - 1, max(urx, px) + 1)
        plt.ylim(min(lly, py) - 1, max(ury, py) + 1)

        # Display the figure in Tkinter
        canvas = FigureCanvasTkAgg(fig, master=self.master)  # A tk.DrawingArea.
        canvas.draw()
        canvas_widget = canvas.get_tk_widget()
        canvas_widget.pack()

        # Update the GUI to ensure the figure is displayed before proceeding
        self.master.update_idletasks()

        # Show result
        self.result_label.config(text="Point inside Rectangle: " + str(result))

        # Ask if the user wants to play again
        self.ask_play_again()

        # After deciding on playing again, remove the canvas to prepare for the next round
        canvas_widget.destroy()

    def ask_play_again(self):
        answer = messagebox.askquestion("Play Again", "Do you want to play again? (Yes/No)")
        if answer == 'no':  # If the user chooses 'No', the application will close
            self.master.quit()

root = tk.Tk()
my_game = GeometryGameGUI(root)
root.mainloop()