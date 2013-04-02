# -*- coding: utf-8 -*-

require "evernote_oauth"

class EvernoteClient
  def initialize(token)
    @token = token
    client = EvernoteOAuth::Client.new(:token => token, :sandbox => false)
    @note_store = client.note_store
  end

  def default_notebook
    @note_store.getDefaultNotebook(@token)
  end

  def valid_guid?(guid)
    guids = notebooks.collect do |notebook|
      notebook.guid
    end

    guids.include?(guid)
  end

  def notebooks
    @note_store.listNotebooks
  end

  def find_notes(options={})
    guid = options[:guid]
    words = options[:words]

    filter = Evernote::EDAM::NoteStore::NoteFilter.new
    filter.notebookGuid ||= guid
    filter.words ||= words

    @note_store.findNotes(@token, filter, 0, 1).notes
  end

  def create_note(notebook_guid, title, content)
    begin
      note = Evernote::EDAM::Type::Note.new
      note.title = title
      note.notebookGuid = notebook_guid
      note.content = content

      @note_store.createNote(@token, note)
    rescue => error
      if error.instance_of?(Evernote::EDAM::Error::EDAMUserException)
        puts("#{error}: Error occured with Evernote server." +
               "#{error.errorCode}: #{error.parameter}")
      else
        p error #for debug
      end
    end
  end

  def update_note(note_guid, title, content)
    note = Evernote::EDAM::Type::Note.new
    note.title = title
    note.guid = note_guid
    note.content = content

    begin
      @note_store.updateNote(@token, note)
    rescue => error
      if error.instance_of?(Evernote::EDAM::Error::EDAMUserException)
        puts("#{error}: Error occured with Evernote server." +
               "#{error.errorCode}: #{error.parameter}")
      else
        p error #for debug
      end
    end
  end

  def note_content(note_guid)
    @note_store.getNoteContent(@token, note_guid)
  end
end
