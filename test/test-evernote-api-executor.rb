# -*- coding: utf-8 -*-
#
# Copyright (C) 2013  Haruka Yoshihara <yshr04hrk@gmail.com>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

require "evernote-api-executor"

class TestEvernoteAPIExecutor < Test::Unit::TestCase
  def setup
    @token = "token"
  end

  def test_default_notebook
    mock_with_evernote_oauth_client do
      mock(EvernoteOAuth::NoteStore).getDefaultNotebook(@token) { "notebook" }
    end

    assert_equal("notebook", EvernoteAPIExecutor.new(@token).default_notebook)
  end

  def test_notebooks
    notebooks = ["Inbox", "Archives", "ToDoList"]
    mock_with_evernote_oauth_client do
      mock(EvernoteOAuth::NoteStore).listNotebooks { notebooks }
    end
    assert_equal(notebooks, EvernoteAPIExecutor.new(@token).notebooks)
  end

  def test_find_notes
    guid    = "test_notebook_guid"
    words   = "test_note"
    content = "This is the test contents. #{words}."
    count   = 10

    filter = generate_filter(guid, words)
    spec = Evernote::EDAM::NoteStore::NotesMetadataResultSpec.new
    notes = [generate_note(guid, "title", content)] * count

    mock_with_evernote_oauth_client do
      mock(EvernoteOAuth::NoteStore).findNotesMetadata(@token, filter, 0, count, spec) do
        mock(Evernote::EDAM::NoteStore::NotesMetadataList).notes do
          metadata = Evernote::EDAM::NoteStore::NoteMetadata.new
          mock(metadata).guid.times(count) { guid }
          [metadata] * count
        end
      end

      getnote_arguments = [@token, guid, true, true, false, false]
      mock(EvernoteOAuth::NoteStore).getNote(*getnote_arguments).times(count) do
        generate_note(guid, "title", content)
      end
    end

    executor = EvernoteAPIExecutor.new(@token)
    actual_notes = executor.find_notes(:guid  => guid,
                                       :words => words,
                                       :count => count)
    assert_equal(notes, actual_notes)
  end

  def test_create_note
    guid    = "test_note_guid"
    title   = "test_note"
    content = "Hello evernote."

    note = generate_note(guid, title, content)

    mock_with_evernote_oauth_client do
      mock(EvernoteOAuth::NoteStore).createNote(@token, note) { note }
    end

    executor = EvernoteAPIExecutor.new(@token)
    actual_note = executor.create_note(guid, title, content)
    assert_equal(note, actual_note)
  end

  private

  def mock_with_evernote_oauth_client
    mock(EvernoteOAuth::Client).new(:token => @token, :sandbox => false) do
      mock(EvernoteOAuth::Client).note_store do
        yield
      end
    end
  end

  def generate_filter(guid, words)
    filter = Evernote::EDAM::NoteStore::NoteFilter.new
    filter.notebookGuid = guid
    filter.words = words
    filter
  end

  def generate_note(guid, title, content)
    note = Evernote::EDAM::Type::Note.new
    note.notebookGuid = guid
    note.title        = title
    note.content      = content
    note
  end
end

