# Generated by Django 4.2.6 on 2023-11-13 22:06

import subprocess

# django settings
from django.conf import settings
from django.core.management import call_command
from django.db import migrations

from jolpica.formula_one.standings import Group, SeasonData


def add_test_data(apps, schema_editor):
    call_command("loaddata", "tests/fixtures/users.json")

    command = [
        "psql",
        "-h",
        settings.DATABASES["default"]["HOST"],
        "-d",
        settings.DATABASES["default"]["NAME"],
        "-U",
        settings.DATABASES["default"]["USER"],
        "-f",
        "tests/fixtures/db/tables.sql",
    ]
    subprocess.run(command, check=True)

    command = [
        "psql",
        "-h",
        settings.DATABASES["default"]["HOST"],
        "-d",
        settings.DATABASES["default"]["NAME"],
        "-U",
        settings.DATABASES["default"]["USER"],
        "-f",
        "tests/fixtures/import.sql",
    ]
    subprocess.run(command, check=True)


def create_driver_standings(apps, schema_editor):
    Season = apps.get_model("formula_one", "Season")
    DriverChampionship = apps.get_model("formula_one", "DriverChampionship")
    TeamChampionship = apps.get_model("formula_one", "TeamChampionship")

    team_standings = []
    driver_standings = []

    for season in Season.objects.all().select_related("championship_system"):
        season_data = SeasonData.from_season(season)
        driver_standings.extend(season_data.generate_standings(Group.DRIVER))
        team_standings.extend(season_data.generate_standings(Group.TEAM))

    DriverChampionship.objects.bulk_create(driver_standings)
    TeamChampionship.objects.bulk_create(team_standings)


class Migration(migrations.Migration):
    dependencies = []

    operations = [
        migrations.RunPython(add_test_data),
        migrations.RunPython(create_driver_standings),
    ]