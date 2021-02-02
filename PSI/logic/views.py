'''
* Grupo 1363
* Pareja 8
* File: views.py
'''

from django.http import HttpResponseForbidden
from django.shortcuts import render
from datamodel.models import Game, Move, Counter, GameStatus
from django.shortcuts import redirect
from django.urls import reverse
from logic.forms import UserForm, MoveForm, SignupForm
from django.contrib.auth import authenticate, login, logout
from django.contrib.auth.decorators import login_required
from django.contrib.auth.password_validation import validate_password
from datamodel import constants
from django.core.exceptions import ValidationError
from django.http import Http404


def anonymous_required(f):
    def wrapped(request):
        if request.user.is_authenticated:
            return HttpResponseForbidden(
                errorHTTP(request, exception="Action restricted to anonymous users"))
        else:
            return f(request)
    return wrapped


def errorHTTP(request, exception=None):
    context_dict = {}
    context_dict[constants.ERROR_MESSAGE_ID] = exception
    return render(request, "mouse_cat/error.html", context_dict).content


def index_service(request):
    return render(request, "mouse_cat/index.html")


@anonymous_required
def login_service(request):

    user_form = UserForm()

    if request.method == 'POST':
        form = UserForm(request.POST)

        username = request.POST.get('username')
        password = request.POST.get('password')

        user = authenticate(username=username, password=password)

        if user:
            if user.is_active:
                login(request, user)
                request.session['counter'] = 0
                return redirect(reverse('landing'))
            else:
                form.add_error(None, 'Usuario/clave no válidos')
                return render(request, 'mouse_cat/login.html', {'user_form': form})
        else:
            print("Invalid login details: {0}, {1}".format(username, password))
            form.add_error(None, 'Usuario/clave no válidos')
            return render(request, 'mouse_cat/login.html', {'user_form': form})

    else:
        return render(request, 'mouse_cat/login.html', {'user_form': user_form})


@login_required
def logout_service(request):
    user = request.user
    logout(request)
    return render(request, 'mouse_cat/logout.html', {'user': user})


@anonymous_required
def signup_service(request):

    user_form = SignupForm()

    if request.method == 'POST':
        user_form = SignupForm(data=request.POST)

        if user_form.is_valid():

            try:
                validate_password(request.POST.get('password'), user=None, password_validators=None)
            except ValidationError as e:
                for error in e.error_list:
                    user_form.add_error(None, error)
                return render(request, 'mouse_cat/signup.html', {'user_form': user_form})

            if request.POST.get('password') != request.POST.get('password2'):
                user_form.add_error(None, 'La clave y su repetición no coinciden.')
                return render(request, 'mouse_cat/signup.html', {'user_form': user_form})

            user = user_form.save()

            if user is not None:

                user.set_password(user.password)
                user.save()

                login(request, user)

                request.session['counter'] = 0

                return redirect(reverse('landing'))

            else:
                user_form.add_error(None, 'Error de registro.')
                return render(request, 'mouse_cat/signup.html', {'user_form': user_form})

        else:
            user_form.add_error(None, 'Usuario duplicado.')
            return render(request, 'mouse_cat/signup.html', {'user_form': user_form})

    else:

        return render(request, 'mouse_cat/signup.html', {'user_form': user_form})


def counter_service(request):

    # get session variable
    if "counter" in request.session:
        request.session["counter"] += 1
    else:
        request.session["counter"] = 1

    global_counter = Counter.objects.inc()

    return render(request, "mouse_cat/counter.html", {'counter_session': request.session["counter"],
                                                      'counter_global': global_counter})


@login_required
def create_game_service(request):
    user = request.user
    game = Game(cat_user=user)

    game.save()

    return render(request, "mouse_cat/new_game.html", {'game': game})


@login_required
def join_game_service(request):

    num_Juegos = len(Game.objects.all())

    if num_Juegos == 0:
        return render(request, "mouse_cat/join_game.html", {'msg_error': constants.JOIN_GAME_ERROR_NOGAME})

    i = 0
    while i < num_Juegos:
        try:
            game = Game.objects.get(id=num_Juegos-i)
        except Game.DoesNotExist:
            return render(request, "mouse_cat/join_game.html", {'msg_error': constants.JOIN_GAME_ERROR_NOGAME})

        if game.status != GameStatus.CREATED:
            i += 1
        else:
            game.mouse_user = request.user
            if game.cat_user == game.mouse_user:
                i += 1
                continue

            game.save()
            return render(request, "mouse_cat/join_game.html", {'game': game})

    return render(request, "mouse_cat/join_game.html", {'msg_error': constants.JOIN_GAME_ERROR_NOGAME})


@login_required
def select_game_service(request, game_id=None):

    if game_id is not None:

        try:
            game = Game.objects.get(id=game_id)
        except Game.DoesNotExist:
            raise Http404("ERROR 404")

        if game.status == GameStatus.CREATED:
            raise Http404("ERROR 404")

        if game.cat_user != request.user and game.mouse_user != request.user:
            raise Http404("ERROR 404")

        request.session[constants.GAME_SELECTED_SESSION_ID] = game_id
        return redirect(reverse('show_game'))

    else:
        game_list_cat = []
        game_list_mouse = []

        num_juegos = len(Game.objects.all())

        if num_juegos == 0:
            return render(request, "mouse_cat/select_game.html", {'as_cat': game_list_cat,
                                                                  'as_mouse': game_list_mouse})

        i = 1
        while i <= num_juegos:
            try:
                game = Game.objects.get(id=i)
            except Game.DoesNotExist:
                return render(request, "mouse_cat/select_game.html", {'as_cat': game_list_cat,
                                                                      'as_mouse': game_list_mouse})

            if game.status == GameStatus.CREATED or game.status == GameStatus.FINISHED:
                i += 1
                continue
            if game.cat_user == request.user:
                game_list_cat.append(game)
            if game.mouse_user == request.user:
                game_list_mouse.append(game)
            i += 1

        return render(request, "mouse_cat/select_game.html", {'as_cat': game_list_cat,
                                                              'as_mouse': game_list_mouse})


@login_required
def show_game_service(request):

    form = MoveForm()

    if 'game_selected' in request.session:
        game_id = request.session['game_selected']
    else:
        return redirect(reverse('landing'))

    game = Game.objects.get(id=game_id)

    tablero = []
    i = 0
    while i < 64:
        tablero.append(0)
        i += 1

    tablero[game.cat1] = 1
    tablero[game.cat2] = 1
    tablero[game.cat3] = 1
    tablero[game.cat4] = 1
    tablero[game.mouse] = -1

    return render(request, "mouse_cat/game.html", {'board': tablero, 'game': game, 'move_form': form})


@login_required
def move_service(request):

    if request.method == 'POST':

        move_form = MoveForm(data=request.POST)

        if move_form.is_valid():
            user = request.user
            if 'game_selected' in request.session:
                game = Game.objects.get(id=request.session['game_selected'])
            else:
                raise Http404("ERROR 404")
            try:
                move = Move(origin=request.POST.get('origin'),
                            target=request.POST.get('target'), game=game, player=user)
                move.save()

                return redirect(reverse('show_game'))

            except ValidationError:
                return redirect(reverse('show_game'))
        else:
            print(move_form.errors)
            return redirect(reverse('landing'))

    raise Http404("ERROR 404")
