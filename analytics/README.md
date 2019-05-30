# Scala Challenge

## What you'll be building

You'll be building a service for aggregating article analytics.  (From now on, we'll refer to articles as *documents*.)  Publishers have JavaScript on their site that tracks every user's interactions.  Every time a user lands on a document, it tracks this visit, including how long they spent reading and how much of the document they completed.  Our service will consume a stream of this tracking data and aggregate it into hourly timeseries for each document.  This means for every document you'll have stored the number of visits, total engaged time, and total completion for any given hour.

## Specification

  - Tracking data is provided to you on a simplified (non-distributed) message queue for you to consume.

  - The message queue holds two different types of message:
      - A visit creation message `VisitCreate`, generated when a user arrives on a document.  It provides a unique ID for the visit, the user making the visit, the document being visited, and the time of the visit.
      - A visit update message `VisitUpdate`, subsequently generated to provide an update on how much time the user has spent reading the document (*engaged time*) in seconds, and how much of it they've completed (*completion*) as a fraction.

  - For each visit, there is exactly one `VisitCreate` and then zero or more subsequent `VisitUpdate` messages.  The `VisitUpdates` don't need to be added — the most recent one provides the full engaged time and completion information to update the visit.

  - There is no termination message for a visit, but `VisitUpdate`s can be received for up to an hour after the associated `VisitCreate` was created.

  - The queue messages are stored as JSON.  Here's an example of a `VisitCreate` message:

    ```
    {
      "messageType": "VisitCreate",
      "visit": {
        "id": "82abce83-3892-48ee-9f1b-d34c4746ace7",
        "userId": "dc0ad841-0b89-4411-a033-d3f174e8d0ad",
        "documentId": "7b2bc74e-f529-4f5d-885b-4377c424211d",
        "createdAt": "2015-04-22T11:42:07.602Z"
      }
    }
    ```

    and an example of a `VisitUpdate` message:

    ```
    {
      "messageType": "VisitUpdate",
      "visit": {
        "id": "82abce83-3892-48ee-9f1b-d34c4746ace7",
        "engagedTime": 25,
        "completion": 0.4,
        "updatedAt": "2015-04-22T11:42:35.122Z"
      }
    }
    ```

  - All IDs are UUIDs that you can assume are unique.  Timestamps are UTC date-times.

  - For each hour, the metrics we'll be tracking are:
      - The number of visits to a document.
      - The number of unique users who visited a document.
      - The total engaged time in hours.
      - The total completion (number of full page views).

  - Document visit analytics are to be aggregated into one-hour time windows according to the visit creation time.  You'll have to decide what data structure to store this.  Ultimately these will be written to a relational database and indexed by document and time.  Here's a tabular representation of what a timeseries for a document might look like:

    | Document      | Start time       | End time         | Visits | Uniques | Time   | Completion |
    | ------------- | ---------------- | ---------------- | ------:| -------:| ------:| ----------:|
    | `7b2bc74e`... | 2015-04-22 13:00 | 2015-04-22 14:00 | 81,172 | 58,593  | 702.91 | 67,399     |
    | `7b2bc74e`... | 2015-04-22 12:00 | 2015-04-22 13:00 | 76,325 | 44,432  | 633.33 | 57,751     |
    | `7b2bc74e`... | 2015-04-22 11:00 | 2015-04-22 12:00 | 72,977 | 40,113  | 598.04 | 51,010     |
    | ⋮             | ⋮                | ⋮                | ⋮      | ⋮       | ⋮      | ⋮          |

    For example, between 12:00 and 13:00 on 22 April 2015, there were 76,325 visits to document `7b2bc74e-f529-4f5d-885b-4377c424211d` with a total of 633.33 hours of engaged time.

  - The main purpose of the service you're building is to consume visit messages from the queues to compute and store the aggregated document timeseries.  You can assume **there will only ever be one instance of the service running**, such that there is no need for the service to be horizontally scalable.

  - Although the aggregated document timeseries would be written to a relational database like PostgreSQL in production, there's no need to implement such functionality.  Instead, just have your service print your stored timeseries to console every time you would write it to the database.

  - It's up to you to decide if and how to perform any logging.

## Queue reading and data

You don't need to read from a real messaging service.  The code we've provided defines an interface `Queue` for a simple read-only message queue and an implementation that provides an FS2 stream of messages from the lines of a file.  Using this is optional, feel free to provide your own implementation with other libraries or frameworks.

You should use the test queue data provided in `test-visit-messages.log`.  This is zipped and available in the repo root directory.
