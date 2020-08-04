#/bin/sh

# detect git tag, branch, commit, and wether the repository is dirty
# when not in a git repository, all the strings will be empty,
# and STklos will be able to detect this.
git_tag="$(git describe --tags --abbrev=0 2>/dev/null || true)"
git_branch="$(git rev-parse --abbrev-ref HEAD 2>/dev/null || true)"
git_commit="$(git rev-parse --short HEAD 2>/dev/null || true)"
git_modified="$(git diff-index --name-only HEAD 2>/dev/null | head | xargs -n 1 printf ' \\"%s\\"')"

cat >git-info.h <<EOF
#define GIT_SUMMARY  "(:branch \"$git_branch\" :commit \"$git_commit\" :tag \"$git_tag\" :modified ($git_modified))"
EOF
