Chapter 1: GRASP Contribution workflow {#chap01}
======================================

In this chapter you will get knowledge about the tools used to manage efficiently
the GRASP inversion code and the way to contribute the code. In this chapter we explain
how to send your contributions. In the next chapter we will elaborate on the types of contributions
and the technical details in order to perform them.

The code is managed by GIT. It is a version control system widely-used. It
is an open source and very powerful one. If you don't know GIT, it is a good moment to get familiar with it, 
once you know it you will never stop using it. 

Additionally, GIT is accompanied by GitLab, a web interface to help users access
the code and organize the community around it. GitLab offers an issue tracking, code reviewer 
and code contribution system (via pull request).

#### Index of content chapter 1:

- 1. [GRASP Contribution workflow](@ref chap01)
   - 1.1. [Code and repository organization]  (@ref chap0101)
   - 1.2. [Open an issue] (@ref chap0102)
   - 1.3. [Forking, contributing and pulling request] (@ref chap0103)

-------

# 1.1. Code and repository organization {#chap0101}

The GRASP code can be extended easily by external code. This external code is integrated
inside the GRASP code folder and compiled together. The following image represents the code organization
of GRASP, where folders and repositories are mixed.

![Partial scheme of code folders and repositories](scheme-of-folder-and-repos.png)

This is also explained in the user documentation: [www.grasp-open.com/doc/ch04.php#multi-repository](http://www.grasp-open.com/doc/ch04.php#multi-repository)

To help preparing the code, there is a tool called grasp-manager. The use of this tool
is explained in the user documentation: [www.grasp-open.com/doc/ch04.php#grasp-manager](http://www.grasp-open.com/doc/ch04.php#grasp-manager)

From the technical point of view, it is important to identify all extensions in order to know where (in which repository)
the code is. Once the repository is identified, the user can contribute by opening an issue or developing a new code
via forking feature. Both options are explained in the following sections.

# 1.2. Open an issue {#chap0102}

A way to support GRASP open is to follow the issue tracker. A user can help answering issues or
opening a new one when a bug is identified. Creating an issue is an easy task, the difficult part is to provide
all required information to help the team resolve the problem. If you are going to open an 
issue, please try to provide us as much information as possible, such as a setting file and sdata file, which help
to reproduce the problem. Last but not least, the user has to identify the proper repository where the issue 
has to be publish. As it is explained in the previous section, the GRASP code is organized with many repositories,
one for each extension. Before opening an issue, please identify the right repository. The following image 
shows the steps to open an issue:

![How to open an issue](issue-screenshot.png)

# 1.3. Forking, contributing and pulling request  {#chap0103}

The forking workflow is the way to contribute to GRASP open editing the repository.
The user can fix a bug, improve documentation or add new features. This is possible by
creating your own fork, modifying it and sending a pull request. This process is explained
in this guide of GitLab: [http://doc.gitlab.com/ee/workflow/forking_workflow.html](http://doc.gitlab.com/ee/workflow/forking_workflow.html)

