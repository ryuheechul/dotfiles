# the purpose of this folder/method is to reduce typing hard ones by avoiding
- captial alphabets
- special chars like `, or ~`
- nested folders

# strategy
- this real folder under dotfiles will not really contain real aliases since aliases will be different for each machine.
- `$ mkdir -p ~/aliases`
- `$ ln -s ~/aliases al`
- add `cd ~/al` at the end of the `~/.zshrc` or `~/.bashrc` if you use bash
- now if you are in `~/al`, time to make some useful aliases that you cd often examples would be

```bash
ln -s /Applications apps
ln -s /Desktop desktop
ln -s /Downloads downloads
ln -s ~/.SpaceVim.d vimconfig
ln -s ~/.config config
ln -s ~/.zshrc zshrc
ln -s ~/dotfiles dot
ln -s ~/dotfiles/aliases/README.md readme.md # to remind yourself for the purpose and edit this README.md anytime
ln -s ~/your/project/that/nested unnested
```
