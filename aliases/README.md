# Why
the purpose of this folder/method is to reduce typing hard ones by avoiding:
- captial alphabets
- special chars like `, or ~`
- nested folders

# Strategy
- this real folder under dotfiles will not really contain real aliases since aliases will be different for each machine.
- `$ mkdir -p ~/aliases`
- `$ ln -sf ~/aliases al`
- add `cd ~/al` at the end of the `~/.zshrc` or `~/.bashrc` if you use bash
- now if you are in `~/al`, time to make some useful aliases that you cd often examples would be

```bash
ln -sf /Applications apps
ln -sf /Desktop desktop
ln -sf /Downloads downloads
ln -sf ~/.SpaceVim.d vimconfig
ln -sf ~/.config config
ln -sf ~/.zshrc zshrc
ln -sf ~/dotfiles dot
ln -sf ~/dotfiles/aliases/README.md readme.md # to remind yourself for the purpose and edit this README.md anytime
ln -sf ~/your/project/that/nested unnested
```
