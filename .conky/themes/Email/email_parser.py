# Copyright (c) 2014, Menno Smits
# Released subject to the New BSD License
# Please see http://en.wikipedia.org/wiki/BSD_licenses
# requires

from __future__ import unicode_literals

from imapclient import IMAPClient
from email.header import decode_header
from email.utils import getaddresses
from tabulate import tabulate
from operator import itemgetter
from datetime import datetime
from humanize import naturaltime, apnumber
from dateutil.tz import tzlocal
from daemon import DaemonContext
import subprocess
import json
import email
import gi
gi.require_version('Notify', '0.7')
from gi.repository import GObject  # noqa
from gi.repository import Notify   # noqa


class EmailNotifier(GObject.Object):
    Notify.init("rss_conky")
    notifications = []

    def __init__(self):
        self.loop = GObject.MainLoop()
        super(EmailNotifier, self).__init__()
        GObject.timeout_add(100, self.exit_when_empty)
        # lets initialise with the application name

    def send_notification(self, title, text, url, file_path_to_icon=""):

        n = Notify.Notification.new(title, text, file_path_to_icon)
        # print('put notification')
        self.notifications.append(n)
        n.add_action('fuck-you', 'open', self.open_mailbrowser)
        n.set_timeout(Notify.EXPIRES_NEVER)
        n.connect('closed', self.close_notification, n)
        n.show()

    def notify_email(self, new_email):
        text = 'Subject: {} | {}'.format(new_email[0], new_email[2])
        title = 'New email from: {}'.format(new_email[1])
        self.send_notification(title, text, 'email')

    def open_mailbrowser(self, n, arg):
        # print(':: webbrowse opening')
        subprocess.Popen(['thunderbird'], stdout=subprocess.PIPE)

    def close_notification(self, n, arg):
        self.notifications.remove(n)
        # print(':: remove notification')
        # print(':: notifications: ', self.notifications)

    def exit_when_empty(self):
        # print('exit check')
        if not any(EmailNotifier.notifications):
            self.loop.quit()
            return False
        return True

example = 'Wed, 11 May 2016 16:35:54 -0300'
date_format = "%a, %d %b %Y %H:%M:%S %z"

HOST = 'imap.mail.com'
USERNAME = 'manoel_vilela@engineer.com'
PASSWORD = '---------'
CACHE_FILE = '.cache.json'
ssl = False


def getmailheader(header_text, default="ascii"):
    """Decode header_text if needed"""
    try:
        headers = decode_header(header_text)
    except email.Errors.HeaderParseError:
        # This already append in email.base64mime.decode()
        # instead return a sanitized ascii string
        return header_text.encode('ascii', 'replace').decode('ascii')
    else:
        for i, (text, charset) in enumerate(headers):
            try:
                headers[i] = str(text, charset or 'utf-8')
            except:
                headers[i] = str(text)
        return u"".join(headers)


def get_emails():
    FETCH_KEY = b'BODY[HEADER.FIELDS (SUBJECT FROM DATE)]'
    server = IMAPClient(HOST, use_uid=True, ssl=ssl)
    server.login(USERNAME, PASSWORD)
    server.select_folder('INBOX', readonly=True)
    messages = server.search(['UNSEEN'])
    response = server.fetch(messages, ['FLAGS', FETCH_KEY])
    emails = []
    for _, data in response.items():
        msg = email.message_from_bytes(data[FETCH_KEY])
        date = msg['date']
        subject = getmailheader(msg["Subject"])
        from_ = ', '.join((x[1] for x in getaddresses([msg['From']])))
        row = [subject, from_, date]
        emails.append(row)

    return emails


def get_cache():
    try:
        with open(CACHE_FILE, 'r', encoding='utf-8') as f:
            return json.load(f)['emails']
    except:
        return []


def put_cache(emails):
    with open(CACHE_FILE, 'w', encoding='utf-8') as f:
        json.dump({'emails': emails}, f)


def notify(new_emails):
    notifier = EmailNotifier()
    for new_email in new_emails:
        notifier.notify_email(new_email)
    notifier.loop.run()


def get_date(d):
    return datetime.strptime(d, date_format)


def prepare_emails(emails):
    now = datetime.now(tzlocal())
    emails_sorted = sorted(emails, key=itemgetter(2), reverse=True)
    emails_prepared = [[s, f, naturaltime(now - get_date(d))]
                       for s, f, d in emails_sorted]
    return emails_prepared


if __name__ == '__main__':
    cache = get_cache()
    emails = get_emails()
    prepared_emails = prepare_emails(emails)
    new_emails = [x for x in emails if x not in cache]

    print('{} new(s) message(s) in INBOX'.format(apnumber(len(emails))))
    if any(emails):
        print(tabulate(prepared_emails, headers=['SUBJECT', 'FROM', 'DATE']))

    put_cache(emails)
    # if any(new_emails):
    #     with DaemonContext():
    notify(new_emails)
