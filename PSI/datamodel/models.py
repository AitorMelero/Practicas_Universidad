'''
* Grupo 1363
* Pareja 8
* File: models.py
'''

from django.db import models
from django.contrib.auth.models import User
from django.core.exceptions import ValidationError
from django.core.validators import MaxValueValidator, MinValueValidator


MSG_ERROR_INVALID_CELL = "Invalid cell for a cat or the mouse|Gato o ratón en posición no válida"
MSG_ERROR_GAMESTATUS = "Game status not valid|Estado no válido"
MSG_ERROR_MOVE = "Move not allowed|Movimiento no permitido"
MSG_ERROR_NEW_COUNTER = "Insert not allowed|Inseción no permitida"
MSG_ERROR_USER_CAT_MOUSE = "No puedes ser raton siendo gato"


class GameStatus(models.Model):
    CREATED = "Created"
    ACTIVE = "Active"
    FINISHED = "Finished"


class Game(models.Model):
    cat_user = models.ForeignKey(User, related_name='games_as_cat', on_delete=models.CASCADE)
    mouse_user = models.ForeignKey(User, related_name='games_as_mouse', on_delete=models.CASCADE, blank=True, null=True)
    cat1 = models.IntegerField(default=0, null=False, validators=[MinValueValidator(0), MaxValueValidator(63)])
    cat2 = models.IntegerField(default=2, null=False, validators=[MinValueValidator(0), MaxValueValidator(63)])
    cat3 = models.IntegerField(default=4, null=False, validators=[MinValueValidator(0), MaxValueValidator(63)])
    cat4 = models.IntegerField(default=6, null=False, validators=[MinValueValidator(0), MaxValueValidator(63)])
    mouse = models.IntegerField(default=59, null=False, validators=[MinValueValidator(0), MaxValueValidator(63)])
    cat_turn = models.BooleanField(default=True, null=False)
    status = models.CharField(max_length=50, default=GameStatus.CREATED, null=False)
    MIN_CELL = 0
    MAX_CELL = 63
    white_cells = [0, 2, 4, 6, 9, 11, 13, 15, 16, 18, 20, 22, 25, 27, 29, 31, 32, 34, 36, 38, 41, 43, 45, 47, 48, 50,
                   52, 54, 57, 59, 61, 63]

    def __str__(self):
        if self.mouse_user is None:
            if self.cat_turn:
                return "(" + str(self.id) + ", " + str(self.status) + ")\t" + "Cat [X] cat_user_test(" + str(
                    self.cat1) + ", " + str(self.cat2) + ", " + str(self.cat3) + ", " + str(self.cat4) + ")"
            else:
                return "(" + str(self.id) + ", " + str(self.status) + ")\t" + "Cat [ ] cat_user_test(" + str(
                    self.cat1) + ", " + str(self.cat2) + ", " + str(self.cat3) + ", " + str(self.cat4) + ")"
        else:
            if self.cat_turn:
                return "(" + str(self.id) + ", " + str(self.status) + ")\t" + "Cat [X] cat_user_test(" + str(
                    self.cat1) + ", " + str(self.cat2) + ", " + str(self.cat3) + ", " + str(self.cat4) + ")" + \
                       " --- Mouse [ ] mouse_user_test(" + str(self.mouse) + ")"
            else:
                return "(" + str(self.id) + ", " + str(self.status) + ")\t" + "Cat [ ] cat_user_test(" + str(
                    self.cat1) + ", " + str(self.cat2) + ", " + str(self.cat3) + ", " + str(self.cat4) + ")" + \
                       " --- Mouse [X] mouse_user_test(" + str(self.mouse) + ")"

    def save(self, *args, **kwargs):
        if self.mouse_user is not None and self.cat_user is not None:
            if self.status == GameStatus.CREATED:
                self.status = GameStatus.ACTIVE

        if self.cat1 < self.MIN_CELL or self.cat2 < self.MIN_CELL or self.cat3 < self.MIN_CELL or \
            self.cat4 < self.MIN_CELL or self.mouse < self.MIN_CELL:
            raise ValidationError(MSG_ERROR_INVALID_CELL)

        if self.cat1 > self.MAX_CELL or self.cat2 > self.MAX_CELL or self.cat3 > self.MAX_CELL or \
            self.cat4 > self.MAX_CELL or self.mouse > self.MAX_CELL:
            raise ValidationError(MSG_ERROR_INVALID_CELL)

        if self.status != GameStatus.CREATED and self.status != GameStatus.ACTIVE and \
            self.status != GameStatus.FINISHED:
            raise ValidationError(MSG_ERROR_GAMESTATUS)

        if self.cat1 not in self.white_cells or self.cat2 not in self.white_cells or \
            self.cat3 not in self.white_cells or self.cat4 not in self.white_cells or \
            self.mouse not in self.white_cells:
            raise ValidationError(MSG_ERROR_INVALID_CELL)

        super(Game, self).save(*args, **kwargs)


class Move(models.Model):
    origin = models.IntegerField(default=0, null=False)
    target = models.IntegerField(default=0, null=False)
    game = models.ForeignKey(Game, on_delete=models.CASCADE, related_name='moves')
    player = models.ForeignKey(User, on_delete=models.CASCADE)
    date = models.DateField(null=False, auto_now=True)

    def save(self, *args, **kwargs):
        if self.game.status != GameStatus.ACTIVE:
            raise ValidationError(MSG_ERROR_MOVE)

        if self.player == self.game.cat_user:

            if self.game.cat_turn:
                if self.game.cat1 != int(self.origin) and self.game.cat2 != int(self.origin) and self.game.cat3 != int(self.origin) \
                    and self.game.cat4 != int(self.origin):
                    raise ValidationError(MSG_ERROR_MOVE)
                if int(self.target) != int(self.origin)+7 and int(self.target) != int(self.origin)+9:
                    raise ValidationError(MSG_ERROR_MOVE)
                if self.game.cat1 == int(self.origin):
                    if int(self.target) == self.game.cat2 or int(self.target) == self.game.cat3 or int(self.target) == self.game.cat4 \
                        or int(self.target) == self.game.mouse:
                        raise ValidationError(MSG_ERROR_MOVE)
                    self.game.cat1 = int(self.target)
                    self.game.cat_turn = False
                    try:
                        self.game.save()
                    except ValidationError:
                        raise ValidationError(MSG_ERROR_MOVE)
                elif self.game.cat2 == int(self.origin):
                    if int(self.target) == self.game.cat1 or int(self.target) == self.game.cat3 or int(self.target) == self.game.cat4 \
                        or int(self.target) == self.game.mouse:
                        raise ValidationError(MSG_ERROR_MOVE)
                    self.game.cat2 = int(self.target)
                    self.game.cat_turn = False
                    try:
                        self.game.save()
                    except ValidationError:
                        raise ValidationError(MSG_ERROR_MOVE)
                elif self.game.cat3 == int(self.origin):
                    if int(self.target) == self.game.cat1 or int(self.target) == self.game.cat2 or int(self.target) == self.game.cat4 \
                        or int(self.target) == self.game.mouse:
                        raise ValidationError(MSG_ERROR_MOVE)
                    self.game.cat3 = int(self.target)
                    self.game.cat_turn = False
                    try:
                        self.game.save()
                    except ValidationError:
                        raise ValidationError(MSG_ERROR_MOVE)
                else:
                    if int(self.target) == self.game.cat1 or int(self.target) == self.game.cat2 or int(self.target) == self.game.cat3 \
                        or int(self.target) == self.game.mouse:
                        raise ValidationError(MSG_ERROR_MOVE)
                    self.game.cat4 = int(self.target)
                    self.game.cat_turn = False
                    try:
                        self.game.save()
                    except ValidationError:
                        raise ValidationError(MSG_ERROR_MOVE)

            else:
                raise ValidationError(MSG_ERROR_MOVE)

        elif self.player == self.game.mouse_user:

            if self.game.cat_turn:
                raise ValidationError(MSG_ERROR_MOVE)

            else:
                if self.game.mouse != int(self.origin):
                    raise ValidationError(MSG_ERROR_MOVE)
                if int(self.target) != int(self.origin)+7 and int(self.target) != int(self.origin)+9 and int(self.target) != int(self.origin)-7\
                    and int(self.target) != int(self.origin)-9:
                    raise ValidationError(MSG_ERROR_MOVE)
                if int(self.target) == self.game.cat1 or int(self.target) == self.game.cat2 or int(self.target) == self.game.cat3\
                    or int(self.target) == self.game.cat4:
                    raise ValidationError(MSG_ERROR_MOVE)
                self.game.mouse = int(self.target)
                self.game.cat_turn = True
                try:
                    self.game.save()
                except ValidationError:
                    raise ValidationError(MSG_ERROR_MOVE)

        else:
            raise ValidationError(MSG_ERROR_MOVE)

        super(Move, self).save(*args, **kwargs)


class CounterObject(models.Manager):

    def inc(self):
        try:
            counter = Counter.objects.get(id=0)
        except Counter.DoesNotExist:
            counter = Counter(id=0)

        counter.value += 1

        super(Counter, counter).save()

        return counter.value

    def get_current_value(self):
        try:
            counter = Counter.objects.get(id=0)
        except Counter.DoesNotExist:
            counter = Counter(id=0)

        return counter.value


class Counter(models.Model):
    value = models.IntegerField(default=0, null=False, validators=[MinValueValidator(0)])
    objects = CounterObject()

    def save(self, *args, **kwargs):
        raise ValidationError(MSG_ERROR_NEW_COUNTER)
