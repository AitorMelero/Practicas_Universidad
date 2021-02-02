'''
* Grupo 1363
* Pareja 8
* File: urls.py
'''

from django.contrib import admin
from django.urls import path
from django.urls import include
from logic import views

urlpatterns = [
                path('', views.index_service, name='landing'),
                path('logic/', include('logic.urls')),
                path('admin/', admin.site.urls),
              ]
