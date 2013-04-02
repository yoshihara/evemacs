# -*- coding: utf-8 -*-
require "evernote-client"

class TestEvernoteClient < Test::Unit::TestCase
  def setup
    @token = "token"
  end

  def test_default_notebook
    mock_with_evernote_oauth_client do
      mock(EvernoteOAuth::NoteStore).getDefaultNotebook(@token) { "notebook" }
    end

    assert_equal("notebook", EvernoteClient.new(@token).default_notebook)
  end

  def test_valid_guid?

  end

  def test_notebooks
    notebook_list = ["Inbox", "Archives", "ToDoList"]
    mock_with_evernote_oauth_client do
      mock(EvernoteOAuth::NoteStore).listNotebooks { notebook_list }
    end
    assert_equal(notebook_list, EvernoteClient.new(@token).notebooks)
  end

  def test_find_notes

  end

  def test_create_note

  end

  def mock_with_evernote_oauth_client
    mock(EvernoteOAuth::Client).new(:token => @token, :sandbox => false) do
      mock(EvernoteOAuth::Client).note_store do
        yield
      end
    end
  end
end

