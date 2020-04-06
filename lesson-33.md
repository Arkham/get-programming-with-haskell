# Capstone: SQL like queries in Haskell

In this capstone we will cover:

- Using the `Monad` type class to create SQL-like queries on lists
- Generalizing functions written for one `Monad` to other ones
- Organizing functions with types

We will build a project called HINQ to query our data relationally.

## Getting started

Let's imagine we have this relational structure:

*Student*
studentId
gradeLevel
studentName

*Teacher*
teacherId
teacherName

*Enrollment*
student
course

*Course*
courseId
courseTitle
teacher

Check `examples/src/HINQ.hs`
