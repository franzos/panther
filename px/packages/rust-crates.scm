;;; Package Repository for GNU Guix
;;; Copyright © 2021-2025 Franz Geffke <mail@gofranz.com>
;;;
;;; guix import -i px/packages/rust-crates.scm crate -f /path/to/Cargo.lock PACKAGE

(define-module (px packages rust-crates)
  #:use-module (guix build-system cargo)
  #:export (lookup-cargo-inputs))

(define rust-ab-glyph-0.2.29
  (crate-source "ab_glyph" "0.2.29"
                "1c7p94qf8x6n62nrbia7nzwdxm2sbyygnhd5qfmfl7p7h30p4dpc"))

(define rust-ab-glyph-0.2.31
  (crate-source "ab_glyph" "0.2.31"
                "0zgf2ax3incf7x0cpm4r82fzhym4mbzjy9i6f5md2655h12lcx70"))

(define rust-ab-glyph-0.2.32
  (crate-source "ab_glyph" "0.2.32"
                "1hkc7y8yjd261d5cm9771dawnwc26rgdlniv3jysb3n3f9s4bh01"))

(define rust-ab-glyph-rasterizer-0.1.10
  (crate-source "ab_glyph_rasterizer" "0.1.10"
                "065n6bj7kqk6f12336lm87fqmvf4lxg7rkg2j56nix228jmgnvrn"))

(define rust-ab-glyph-rasterizer-0.1.8
  (crate-source "ab_glyph_rasterizer" "0.1.8"
                "0ikhgzig59q8b1a1iw83sxfnvylg5gx6w2y8ynbnf231xs9if6y7"))

(define rust-abnf-core-0.6.0
  (crate-source "abnf-core" "0.6.0"
                "1shdbi2ffzvyf6v3wwlgwp5sa58m22pqk716b6gnm40v0wgjs67c"))

(define rust-actix-codec-0.5.2
  (crate-source "actix-codec" "0.5.2"
                "12m2jxysk2xpxi193340zv4w215cv9fyyna7rxvzh6wck0hhlysz"))

(define rust-actix-http-3.11.2
  (crate-source "actix-http" "3.11.2"
                "0jds7l5dx1fvwbk84ki2r8mx6fxlg8w1wwqk3xfzpqnb2h1qc9kr"))

(define rust-actix-macros-0.2.4
  (crate-source "actix-macros" "0.2.4"
                "1jsmhq9k5nsms8sci2lqkihym5nrhlpfv8dgd0n4539g1cad67p0"))

(define rust-actix-router-0.5.3
  (crate-source "actix-router" "0.5.3"
                "1y1n086zgfgf6483vlm18651n5ga6rcvlwvynmkkixji9hb29lqk"))

(define rust-actix-rt-2.11.0
  (crate-source "actix-rt" "2.11.0"
                "0qwck5j9xyfknldpbv3y35ssi9h71r7p66ga4rv9m9cchwa9fn4j"))

(define rust-actix-server-2.6.0
  (crate-source "actix-server" "2.6.0"
                "00kmzwcr0vyb4q6m7sch2xr3px2vd560p8zvy83syzj59bm68l56"))

(define rust-actix-service-2.0.3
  (crate-source "actix-service" "2.0.3"
                "0zyw2178kcyy3r775klvkb3riai1sjxnzcxxqjyl9bz5y1mz6ily"))

(define rust-actix-utils-3.0.1
  (crate-source "actix-utils" "3.0.1"
                "1n05nzwdkx6jhmzr6f9qsh57a8hqlwv5rjz1i0j3qvj6y7gxr8c8"))

(define rust-actix-web-4.12.1
  (crate-source "actix-web" "4.12.1"
                "1mmqwnprcrgba75m3g7w26pi8ic6bxlfb8rpjq27zqs2l5xsfm0n"))

(define rust-actix-web-codegen-4.3.0
  (crate-source "actix-web-codegen" "4.3.0"
                "1f12ss948vpanh98a1v7f2x893g7xfh1mpgiz9fhnjb85q73i4gm"))

(define rust-addr2line-0.19.0
  (crate-source "addr2line" "0.19.0"
                "15ywmr5wx22q69ffnn79qp65ir5p1x0k2q06plcpv6v74c5xcvx7"))

(define rust-addr2line-0.21.0
  (crate-source "addr2line" "0.21.0"
                "1jx0k3iwyqr8klqbzk6kjvr496yd94aspis10vwsj5wy7gib4c4a"))

(define rust-addr2line-0.22.0
  (crate-source "addr2line" "0.22.0"
                "0y66f1sa27i9kvmlh76ynk60rxfrmkba9ja8x527h32wdb206ibf"))

(define rust-addr2line-0.24.2
  (crate-source "addr2line" "0.24.2"
                "1hd1i57zxgz08j6h5qrhsnm2fi0bcqvsh389fw400xm3arz2ggnz"))

(define rust-addr2line-0.25.1
  (crate-source "addr2line" "0.25.1"
                "0jwb96gv17vdr29hbzi0ha5q6jkpgjyn7rjlg5nis65k41rk0p8v"))

(define rust-adler-1.0.2
  (crate-source "adler" "1.0.2"
                "1zim79cvzd5yrkzl3nyfx0avijwgk9fqv3yrscdy1cc79ih02qpj"))

(define rust-adler2-2.0.0
  (crate-source "adler2" "2.0.0"
                "09r6drylvgy8vv8k20lnbvwq8gp09h7smfn6h1rxsy15pgh629si"))

(define rust-adler2-2.0.1
  (crate-source "adler2" "2.0.1"
                "1ymy18s9hs7ya1pjc9864l30wk8p2qfqdi7mhhcc5nfakxbij09j"))

(define rust-adler32-1.2.0
  (crate-source "adler32" "1.2.0"
                "0d7jq7jsjyhsgbhnfq5fvrlh9j0i9g1fqrl2735ibv5f75yjgqda"))

(define rust-aes-0.8.4
  (crate-source "aes" "0.8.4"
                "1853796anlwp4kqim0s6wm1srl4ib621nm0cl2h3c8klsjkgfsdi"))

(define rust-ahash-0.7.8
  (crate-source "ahash" "0.7.8"
                "1y9014qsy6gs9xld4ch7a6xi9bpki8vaciawxq4p75d8qvh7f549"))

(define rust-ahash-0.8.11
  (crate-source "ahash" "0.8.11"
                "04chdfkls5xmhp1d48gnjsmglbqibizs3bpbj6rsj604m10si7g8"))

(define rust-ahash-0.8.12
  (crate-source "ahash" "0.8.12"
                "0xbsp9rlm5ki017c0w6ay8kjwinwm8knjncci95mii30rmwz25as"))

(define rust-ahash-0.8.3
  (crate-source "ahash" "0.8.3"
                "0bzcsxdl2wd6j2p4214qh9sqkqn69gi7f9lk1xi8yj063r6zd69c"))

(define rust-aho-corasick-1.0.4
  (crate-source "aho-corasick" "1.0.4"
                "0niskpw2ajzn0gf8q99hypzn7mrc2bs03a3gk4a4vva8yggfhj37"))

(define rust-aho-corasick-1.1.2
  (crate-source "aho-corasick" "1.1.2"
                "1w510wnixvlgimkx1zjbvlxh6xps2vjgfqgwf5a6adlbjp5rv5mj"))

(define rust-aho-corasick-1.1.3
  (crate-source "aho-corasick" "1.1.3"
                "05mrpkvdgp5d20y2p989f187ry9diliijgwrs254fs9s1m1x6q4f"))

(define rust-aho-corasick-1.1.4
  (crate-source "aho-corasick" "1.1.4"
                "00a32wb2h07im3skkikc495jvncf62jl6s96vwc7bhi70h9imlyx"))

(define rust-aliasable-0.1.3
  (crate-source "aliasable" "0.1.3"
                "1z8548zdjlm4ps1k0d7x68lfdyji02crwcc9rw3q3bb106f643r5"))

(define rust-aligned-vec-0.6.4
  (crate-source "aligned-vec" "0.6.4"
                "16vnf78hvfix5cwzd5xs5a2g6afmgb4h7n6yfsc36bv0r22072fw"))

(define rust-alloc-no-stdlib-2.0.4
  (crate-source "alloc-no-stdlib" "2.0.4"
                "1cy6r2sfv5y5cigv86vms7n5nlwhx1rbyxwcraqnmm1rxiib2yyc"))

(define rust-alloc-stdlib-0.2.2
  (crate-source "alloc-stdlib" "0.2.2"
                "1kkfbld20ab4165p29v172h8g0wvq8i06z8vnng14whw0isq5ywl"))

(define rust-allocator-api2-0.2.21
  (crate-source "allocator-api2" "0.2.21"
                "08zrzs022xwndihvzdn78yqarv2b9696y67i6h78nla3ww87jgb8"))

(define rust-alsa-0.9.1
  (crate-source "alsa" "0.9.1"
                "0hvxc447bsynyhzhmznw6w2kwbid83p712dls4h1x8w3pavp4xgd"))

(define rust-alsa-sys-0.3.1
  ;; TODO: Check bundled sources.
  (crate-source "alsa-sys" "0.3.1"
                "09qmmnpmlcj23zcgx2xsi4phcgm5i02g9xaf801y7i067mkfx3yv"))

(define rust-android-activity-0.6.0
  (crate-source "android-activity" "0.6.0"
                "0inh88x8x2fh62jg739s9hwyvdh8i920qf0qw7bhr802j9c7hsgg"))

(define rust-android-properties-0.2.2
  (crate-source "android-properties" "0.2.2"
                "016slvg269c0y120p9qd8vdfqa2jbw4j0g18gfw6p3ain44v4zpw"))

(define rust-android-system-properties-0.1.5
  (crate-source "android_system_properties" "0.1.5"
                "04b3wrz12837j7mdczqd95b732gw5q7q66cv4yn4646lvccp57l1"))

(define rust-android-tzdata-0.1.1
  (crate-source "android-tzdata" "0.1.1"
                "1w7ynjxrfs97xg3qlcdns4kgfpwcdv824g611fq32cag4cdr96g9"))

(define rust-anes-0.1.6
  (crate-source "anes" "0.1.6"
                "16bj1ww1xkwzbckk32j2pnbn5vk6wgsl3q4p3j9551xbcarwnijb"))

(define rust-ansi-colours-1.2.3
  (crate-source "ansi_colours" "1.2.3"
                "1zimwh84gs1r0g0chy6x5lm9v0ksxxlzwy8nyj80f6cq08zc9vhl"))

(define rust-ansi-to-tui-8.0.1
  (crate-source "ansi-to-tui" "8.0.1"
                "1zb5vjm2fj3yvnylkjjp29y9k4fhnbhqa2lgylmh93wmknxnc8z4"))

(define rust-anstream-0.6.15
  (crate-source "anstream" "0.6.15"
                "09nm4qj34kiwgzczdvj14x7hgsb235g4sqsay3xsz7zqn4d5rqb4"))

(define rust-anstream-0.6.18
  (crate-source "anstream" "0.6.18"
                "16sjk4x3ns2c3ya1x28a44kh6p47c7vhk27251i015hik1lm7k4a"))

(define rust-anstream-0.6.20
  (crate-source "anstream" "0.6.20"
                "14k1iqdf3dx7hdjllmql0j9sjxkwr1lfdddi3adzff0r7mjn7r9s"))

(define rust-anstream-0.6.21
  (crate-source "anstream" "0.6.21"
                "0jjgixms4qjj58dzr846h2s29p8w7ynwr9b9x6246m1pwy0v5ma3"))

(define rust-anstream-0.6.7
  (crate-source "anstream" "0.6.7"
                "1fkmc8vh3bwd5mycrkhzclax37yd4j374kdpj0lspyn179dl1ljc"))

(define rust-anstyle-1.0.1
  (crate-source "anstyle" "1.0.1"
                "1kff80219d5rvvi407wky2zdlb0naxvbbg005s274pidbxfdlc1s"))

(define rust-anstyle-1.0.10
  (crate-source "anstyle" "1.0.10"
                "1yai2vppmd7zlvlrp9grwll60knrmscalf8l2qpfz8b7y5lkpk2m"))

(define rust-anstyle-1.0.11
  (crate-source "anstyle" "1.0.11"
                "1gbbzi0zbgff405q14v8hhpi1kz2drzl9a75r3qhks47lindjbl6"))

(define rust-anstyle-1.0.13
  (crate-source "anstyle" "1.0.13"
                "0y2ynjqajpny6q0amvfzzgw0gfw3l47z85km4gvx87vg02lcr4ji"))

(define rust-anstyle-1.0.8
  (crate-source "anstyle" "1.0.8"
                "1cfmkza63xpn1kkz844mgjwm9miaiz4jkyczmwxzivcsypk1vv0v"))

(define rust-anstyle-parse-0.2.1
  (crate-source "anstyle-parse" "0.2.1"
                "0cy38fbdlnmwyy6q8dn8dcfrpycwnpjkljsjqn3kmc40b7zp924k"))

(define rust-anstyle-parse-0.2.5
  (crate-source "anstyle-parse" "0.2.5"
                "1jy12rvgbldflnb2x7mcww9dcffw1mx22nyv6p3n7d62h0gdwizb"))

(define rust-anstyle-parse-0.2.6
  (crate-source "anstyle-parse" "0.2.6"
                "1acqayy22fwzsrvr6n0lz6a4zvjjcvgr5sm941m7m0b2fr81cb9v"))

(define rust-anstyle-parse-0.2.7
  (crate-source "anstyle-parse" "0.2.7"
                "1hhmkkfr95d462b3zf6yl2vfzdqfy5726ya572wwg8ha9y148xjf"))

(define rust-anstyle-query-1.0.0
  (crate-source "anstyle-query" "1.0.0"
                "0js9bgpqz21g0p2nm350cba1d0zfyixsma9lhyycic5sw55iv8aw"))

(define rust-anstyle-query-1.1.1
  (crate-source "anstyle-query" "1.1.1"
                "0aj22iy4pzk6mz745sfrm1ym14r0y892jhcrbs8nkj7nqx9gqdkd"))

(define rust-anstyle-query-1.1.2
  (crate-source "anstyle-query" "1.1.2"
                "036nm3lkyk43xbps1yql3583fp4hg3b1600is7mcyxs1gzrpm53r"))

(define rust-anstyle-query-1.1.4
  (crate-source "anstyle-query" "1.1.4"
                "1qir6d6fl5a4y2gmmw9a5w93ckwx6xn51aryd83p26zn6ihiy8wy"))

(define rust-anstyle-query-1.1.5
  (crate-source "anstyle-query" "1.1.5"
                "1p6shfpnbghs6jsa0vnqd8bb8gd7pjd0jr7w0j8jikakzmr8zi20"))

(define rust-anstyle-wincon-3.0.1
  (crate-source "anstyle-wincon" "3.0.1"
                "0a066gr4p7bha8qwnxyrpbrqzjdvk8l7pdg7isljimpls889ssgh"))

(define rust-anstyle-wincon-3.0.10
  (crate-source "anstyle-wincon" "3.0.10"
                "0ajz9wsf46a2l3pds7v62xbhq2cffj7wrilamkx2z8r28m0k61iy"))

(define rust-anstyle-wincon-3.0.11
  (crate-source "anstyle-wincon" "3.0.11"
                "0zblannm70sk3xny337mz7c6d8q8i24vhbqi42ld8v7q1wjnl7i9"))

(define rust-anstyle-wincon-3.0.4
  (crate-source "anstyle-wincon" "3.0.4"
                "1y2pkvsrdxbcwircahb4wimans2pzmwwxad7ikdhj5lpdqdlxxsv"))

(define rust-anstyle-wincon-3.0.6
  (crate-source "anstyle-wincon" "3.0.6"
                "099ir0w3lbpsp1nxdzbf4anq98ww8ykyc9pd1g03xgkj1v7dn291"))

(define rust-anstyle-wincon-3.0.7
  (crate-source "anstyle-wincon" "3.0.7"
                "0kmf0fq4c8yribdpdpylzz1zccpy84hizmcsac3wrac1f7kk8dfa"))

(define rust-anyhow-1.0.100
  (crate-source "anyhow" "1.0.100"
                "0qbfmw4hhv2ampza1csyvf1jqjs2dgrj29cv3h3sh623c6qvcgm2"))

(define rust-anyhow-1.0.75
  (crate-source "anyhow" "1.0.75"
                "1rmcjkim91c5mw7h9wn8nv0k6x118yz0xg0z1q18svgn42mqqrm4"))

(define rust-anyhow-1.0.79
  (crate-source "anyhow" "1.0.79"
                "1ji5irqiwr8yprgqj8zvnli7zd7fz9kzaiddq44jnrl2l289h3h8"))

(define rust-anyhow-1.0.86
  (crate-source "anyhow" "1.0.86"
                "1nk301x8qhpdaks6a9zvcp7yakjqnczjmqndbg7vk4494d3d1ldk"))

(define rust-anyhow-1.0.92
  (crate-source "anyhow" "1.0.92"
                "04xgvbdlgcfd80ahvgsk1dq9q4969ilj9n4xp62052nlsxk73wvl"))

(define rust-anyhow-1.0.96
  (crate-source "anyhow" "1.0.96"
                "1x0b2lk76lfgj069jadmn9zi1wscwz45nwfjgnvbdnc99qc4v5kb"))

(define rust-anyhow-1.0.97
  (crate-source "anyhow" "1.0.97"
                "0kvspbiwncmmkdgrwjrimsmbmhzxc641p5ql99l2rjq6smmdbznw"))

(define rust-anyhow-1.0.99
  (crate-source "anyhow" "1.0.99"
                "001icqvkfl28rxxmk99rm4gvdzxqngj5v50yg2bh3dzcvqfllrxh"))

(define rust-approx-0.5.1
  (crate-source "approx" "0.5.1"
                "1ilpv3dgd58rasslss0labarq7jawxmivk17wsh8wmkdm3q15cfa"))

(define rust-arbitrary-1.4.2
  (crate-source "arbitrary" "1.4.2"
                "1wcbi4x7i3lzcrkjda4810nqv03lpmvfhb0a85xrq1mbqjikdl63"))

(define rust-arboard-3.6.1
  (crate-source "arboard" "3.6.1"
                "1byx6q5iipxkb0pyjp80k7c4akp4n5m7nsmqdbz4n7s9ak0a2j03"))

(define rust-arc-swap-1.7.1
  (crate-source "arc-swap" "1.7.1"
                "0mrl9a9r9p9bln74q6aszvf22q1ijiw089jkrmabfqkbj31zixv9"))

(define rust-arc-swap-1.8.0
  (crate-source "arc-swap" "1.8.0"
                "0zi02pwgn0vj615k6gpsr36fa6ix8qqsys88ywpcr8lcpd4k9l2i"))

(define rust-arg-enum-proc-macro-0.3.4
  (crate-source "arg_enum_proc_macro" "0.3.4"
                "1sjdfd5a8j6r99cf0bpqrd6b160x9vz97y5rysycsjda358jms8a"))

(define rust-argh-0.1.13
  (crate-source "argh" "0.1.13"
                "0h6jzj4aqswk9x6w3lbb8kdskyf93v73wlrfk4pvhdlabhr1izrl"))

(define rust-argh-derive-0.1.13
  (crate-source "argh_derive" "0.1.13"
                "00vqfqgxqq6dd9jgbg9qhn12hh06qzsj1incv3ajklsh7awb5dxd"))

(define rust-argh-shared-0.1.13
  (crate-source "argh_shared" "0.1.13"
                "1xplhinnv139x2w2wknvnms7css6c99l8dw7jb1wvv9dr0y18r54"))

(define rust-ariadne-0.2.0
  (crate-source "ariadne" "0.2.0"
                "00jprfhqydhdnwibjzbcv7g0aj5zbyy0fislhz88hx9hhynx0zrn"))

(define rust-array-concat-0.5.5
  (crate-source "array-concat" "0.5.5"
                "10gwa7k4aypg3jkfz51hc9ix71g3hadaz9d2in2m3n12xg7ng46y"))

(define rust-arraydeque-0.5.1
  (crate-source "arraydeque" "0.5.1"
                "0dn2xdfg3rkiqsh8a6achnmvf5nf11xk33xgjzpksliab4yjx43x"))

(define rust-arrayref-0.3.9
  (crate-source "arrayref" "0.3.9"
                "1jzyp0nvp10dmahaq9a2rnxqdd5wxgbvp8xaibps3zai8c9fi8kn"))

(define rust-arrayvec-0.7.6
  (crate-source "arrayvec" "0.7.6"
                "0l1fz4ccgv6pm609rif37sl5nv5k6lbzi7kkppgzqzh1vwix20kw"))

(define rust-as-raw-xcb-connection-1.0.1
  (crate-source "as-raw-xcb-connection" "1.0.1"
                "0sqgpz2ymv5yx76r5j2npjq2x5qvvqnw0vrs35cyv30p3pfp2m8p"))

(define rust-ash-0.37.3+1.3.251
  (crate-source "ash" "0.37.3+1.3.251"
                "0jndbsi5c8xifh4fdp378xpbyzdhs7y38hmbhih0lsv8bn1w7s9r"))

(define rust-ash-0.38.0+1.3.281
  (crate-source "ash" "0.38.0+1.3.281"
                "0vx4yf689v1rc680jvy8bnysx5sgd8f33wnp2vqaizh0v0v4kd0b"))

(define rust-ashpd-0.10.2
  (crate-source "ashpd" "0.10.2"
                "1cw0ngs9037w3sb61vb67vzf7n48yhwma080xmmwrnqlfrq9vhz9"))

(define rust-ashpd-0.11.0
  (crate-source "ashpd" "0.11.0"
                "1pwrxm1dky7i0xb1wj5k5rfx7hhx06vks2d2dvpamlvzsw8g7gbc"))

(define rust-asn1-rs-0.7.1
  (crate-source "asn1-rs" "0.7.1"
                "0q0ydbjh2cawwic3r9rfk6lyas2qnj6f2aiic5nw5f1bi2b4lqjn"))

(define rust-asn1-rs-derive-0.6.0
  (crate-source "asn1-rs-derive" "0.6.0"
                "0b7fpyjs2kyb2i922br5mbg8rml46rihr8qmcpdyj2a93sdy829i"))

(define rust-asn1-rs-impl-0.2.0
  (crate-source "asn1-rs-impl" "0.2.0"
                "1xv56m0wrwix4av3w86sih1nsa5g1dgfz135lz1qdznn5h60a63v"))

(define rust-assert-cmd-2.0.16
  (crate-source "assert_cmd" "2.0.16"
                "0gdj0710k3lnvyjmpv8a4dgwrk9ib85l2wfw4n2xwy3qyavka66w"))

(define rust-assert-cmd-2.1.1
  (crate-source "assert_cmd" "2.1.1"
                "11dz6ljl8v9alh6mcbhdp0niibgxraxhibs24i1y1a8aacj6kfxw"))

(define rust-assert-matches-1.5.0
  (crate-source "assert_matches" "1.5.0"
                "1a9b3p9vy0msylyr2022sk5flid37ini1dxji5l3vwxsvw4xcd4v"))

(define rust-async-broadcast-0.5.1
  (crate-source "async-broadcast" "0.5.1"
                "0avdqbci1qdlfc4glc3wqrb0wi5ffc7bqv2q1wg14syayvdwqj3w"))

(define rust-async-broadcast-0.7.2
  (crate-source "async-broadcast" "0.7.2"
                "0ckmqcwyqwbl2cijk1y4r0vy60i89gqc86ijrxzz5f2m4yjqfnj3"))

(define rust-async-channel-1.8.0
  (crate-source "async-channel" "1.8.0"
                "0cv8ha6y7s6scsgjipn7ciybry9g57rklw8404igrksw7vlgwing"))

(define rust-async-channel-2.3.1
  (crate-source "async-channel" "2.3.1"
                "0skvwxj6ysfc6d7bhczz9a2550260g62bm5gl0nmjxxyn007id49"))

(define rust-async-channel-2.5.0
  (crate-source "async-channel" "2.5.0"
                "1ljq24ig8lgs2555myrrjighycpx2mbjgrm3q7lpa6rdsmnxjklj"))

(define rust-async-executor-1.13.1
  (crate-source "async-executor" "1.13.1"
                "1v6w1dbvsmw6cs4dk4lxj5dvrikc6xi479wikwaab2qy3h09mjih"))

(define rust-async-executor-1.13.2
  (crate-source "async-executor" "1.13.2"
                "1yjgdh62iqn2bvp792j3q980kk015x79fz8dhq8dsjsjb3xjz0dv"))

(define rust-async-executor-1.13.3
  (crate-source "async-executor" "1.13.3"
                "1f3za9v8wkqzv6rz69g0qzvdcmghwbixijwzldwjm9w3zph00z29"))

(define rust-async-executor-1.5.1
  (crate-source "async-executor" "1.5.1"
                "1yrs723d4djhia5mimc85blrvly0kl8bj260b5vz0r4559gxr8vg"))

(define rust-async-fs-1.6.0
  (crate-source "async-fs" "1.6.0"
                "01if2h77mry9cnm91ql2md595108i2c1bfy9gaivzvjfcl2gk717"))

(define rust-async-fs-2.1.2
  (crate-source "async-fs" "2.1.2"
                "0jp0p7lg9zqy2djgdmivbzx0yqmfn9sm2s9dkhaws3zlharhkkgb"))

(define rust-async-fs-2.2.0
  (crate-source "async-fs" "2.2.0"
                "1iclw9970mh4ndb0bd68a6901kqy81rf9yypvf78pvaavy0scd40"))

(define rust-async-io-1.13.0
  (crate-source "async-io" "1.13.0"
                "1byj7lpw0ahk6k63sbc9859v68f28hpaab41dxsjj1ggjdfv9i8g"))

(define rust-async-io-2.4.0
  (crate-source "async-io" "2.4.0"
                "0n8h0vy53n4vdkq529scqnkzm9vcl3r73za9nj81s2nfrhiv78j3"))

(define rust-async-io-2.4.1
  (crate-source "async-io" "2.4.1"
                "1cybmbq3gwv26bi7sk1gmcwynbm1s36zj3wib1gpdwx0fnpc0dqj"))

(define rust-async-io-2.6.0
  (crate-source "async-io" "2.6.0"
                "1z16s18bm4jxlmp6rif38mvn55442yd3wjvdfhvx4hkgxf7qlss5"))

(define rust-async-lock-2.7.0
  (crate-source "async-lock" "2.7.0"
                "1mrd4kai92fcgl9974dpmibiq6ja9drz41v3crvv0c27a8kzf97s"))

(define rust-async-lock-3.4.0
  (crate-source "async-lock" "3.4.0"
                "060vh45i809wcqyxzs5g69nqiqah7ydz0hpkcjys9258vqn4fvpz"))

(define rust-async-lock-3.4.1
  (crate-source "async-lock" "3.4.1"
                "1p6i1sw3mwv1msdx9jqkr0h0a2jlrp3717yyx5n9pvkw0h23dl2z"))

(define rust-async-net-2.0.0
  (crate-source "async-net" "2.0.0"
                "1xyc5a5vcp3a7h1q2lbfh79wz8136dig4q4x6g4w2ws8ml7h0j5r"))

(define rust-async-pidfd-0.1.4
  (crate-source "async-pidfd" "0.1.4"
                "168pylpf7n898szw32sva7kf9h3x1mnip54mfr8f7f4v55c705qj"))

(define rust-async-pidfd-0.1.5
  (crate-source "async-pidfd" "0.1.5"
                "05qc4d15yr6m3j5zwprdvn8brwqnb2pkg540y6ip7xcn72i230wm"))

(define rust-async-process-1.7.0
  (crate-source "async-process" "1.7.0"
                "1jcra7vqidszfgy5c1v0d7xflm3qsh857r722999223yv6qji7bs"))

(define rust-async-process-2.3.0
  (crate-source "async-process" "2.3.0"
                "1fr6cpqdw7hrmzns1983lgx86cg8vyz7nlrn0h0125iqq8fmy9b3"))

(define rust-async-process-2.3.1
  (crate-source "async-process" "2.3.1"
                "1p459ckyndgl69cdxl8375cwnm6wsv5ra069zyndf8b01vjg9qyd"))

(define rust-async-process-2.5.0
  (crate-source "async-process" "2.5.0"
                "0xfswxmng6835hjlfhv7k0jrfp7czqxpfj6y2s5dsp05q0g94l7w"))

(define rust-async-recursion-1.0.4
  (crate-source "async-recursion" "1.0.4"
                "1fhwz7jqgsakbjsr2nrsvgs245l1m5dkzir6f9fxw4ngwrywx5qf"))

(define rust-async-recursion-1.1.1
  (crate-source "async-recursion" "1.1.1"
                "04ac4zh8qz2xjc79lmfi4jlqj5f92xjvfaqvbzwkizyqd4pl4hrv"))

(define rust-async-signal-0.2.10
  (crate-source "async-signal" "0.2.10"
                "1wxrq3871l00mil43nmh0akvwjjjnv0bn7n2pzwbvh00k0s00zk3"))

(define rust-async-signal-0.2.11
  (crate-source "async-signal" "0.2.11"
                "03ffmhkvr4si21rlnhxhb68fslfybyzp0nldi7rnvc6la175lq6p"))

(define rust-async-signal-0.2.13
  (crate-source "async-signal" "0.2.13"
                "0k66mpb3xp86hj4vxs7w40v7qz2jfbblrm9ddc5mglwwynxp1h23"))

(define rust-async-stream-0.3.6
  (crate-source "async-stream" "0.3.6"
                "0xl4zqncrdmw2g6241wgr11dxdg4h7byy6bz3l6si03qyfk72nhb"))

(define rust-async-stream-impl-0.3.6
  (crate-source "async-stream-impl" "0.3.6"
                "0kaplfb5axsvf1gfs2gk6c4zx6zcsns0yf3ssk7iwni7bphlvhn7"))

(define rust-async-task-4.4.0
  (crate-source "async-task" "4.4.0"
                "1bis5aqnjw3gxridzpq2cwrlj4y8alrfqy19rly6ag2vh50spizc"))

(define rust-async-task-4.7.1
  (crate-source "async-task" "4.7.1"
                "1pp3avr4ri2nbh7s6y9ws0397nkx1zymmcr14sq761ljarh3axcb"))

(define rust-async-trait-0.1.68
  (crate-source "async-trait" "0.1.68"
                "0hp8ysdjr8c43avm7bkj73cd22ra3dpzag82bjyyj6qn5a7xvk5r"))

(define rust-async-trait-0.1.73
  (crate-source "async-trait" "0.1.73"
                "1w60x18qm18drm8pdl0ix4jai83nh8hlwfjswca3dh4096rww05w"))

(define rust-async-trait-0.1.80
  (crate-source "async-trait" "0.1.80"
                "1jip2xkv8l67bbg6jrz3b1sdb7api77vy38wrjl7sfkmya3j1yn6"))

(define rust-async-trait-0.1.81
  (crate-source "async-trait" "0.1.81"
                "01w10ad3984czxrmc8ckdrabhmsv80aynfxibjnqwz1dr3f2h33f"))

(define rust-async-trait-0.1.83
  (crate-source "async-trait" "0.1.83"
                "1p8q8gm4fv2fdka8hwy2w3f8df7p5inixqi7rlmbnky3wmysw73j"))

(define rust-async-trait-0.1.85
  (crate-source "async-trait" "0.1.85"
                "0mm0gwad44zs7mna4a0m1z4dhzpmydfj73w4wm23c8xpnhrli4rz"))

(define rust-async-trait-0.1.87
  (crate-source "async-trait" "0.1.87"
                "15swwmyl4nx7w03rq6ibb4x2c8rzbx9fpiag1kn4fhapb49yqmnm"))

(define rust-async-trait-0.1.88
  (crate-source "async-trait" "0.1.88"
                "1dgxvz7g75cmz6vqqz0mri4xazc6a8xfj1db6r9fxz29lzyd6fg5"))

(define rust-async-trait-0.1.89
  (crate-source "async-trait" "0.1.89"
                "1fsxxmz3rzx1prn1h3rs7kyjhkap60i7xvi0ldapkvbb14nssdch"))

(define rust-atk-0.18.0
  (crate-source "atk" "0.18.0"
                "1r0pdczm9jyh6lpn5hc3nmpbmp8ilbab52d6kypyi06x2x5h3bxl"))

(define rust-atk-sys-0.18.0
  ;; TODO: Check bundled sources.
  (crate-source "atk-sys" "0.18.0"
                "0280k0xw21n3zzri8ynk5mxy7v1mk9d506l962lhngp3j1yhn7i5"))

(define rust-atomic-0.6.0
  (crate-source "atomic" "0.6.0"
                "15193mfhmrq3p6vi1a10hw3n6kvzf5h32zikhby3mdj0ww1q10cd"))

(define rust-atomic-0.6.1
  (crate-source "atomic" "0.6.1"
                "0h43ljcgbl6vk62hs6yk7zg7qn3myzvpw8k7isb9nzhkbdvvz758"))

(define rust-atomic-waker-1.1.1
  (crate-source "atomic-waker" "1.1.1"
                "1lr6gzsmxs49fp1kjjngz6asfz8ncxfpkbmi7nh9drzws7hf308i"))

(define rust-atomic-waker-1.1.2
  (crate-source "atomic-waker" "1.1.2"
                "1h5av1lw56m0jf0fd3bchxq8a30xv0b4wv8s4zkp4s0i7mfvs18m"))

(define rust-auditable-serde-0.8.0
  (crate-source "auditable-serde" "0.8.0"
                "1iaqsnf4mvrmc7b8nwf7fv7mzk5md7hl724hvxc04g7w7laghysw"))

(define rust-auto-impl-1.2.0
  (crate-source "auto_impl" "1.2.0"
                "0hmfcahj0vrnzq7rayk7r428zp54x9a8awgw6wil753pbvqz71rw"))

(define rust-autocfg-1.1.0
  (crate-source "autocfg" "1.1.0"
                "1ylp3cb47ylzabimazvbz9ms6ap784zhb6syaz6c1jqpmcmq0s6l"))

(define rust-autocfg-1.3.0
  (crate-source "autocfg" "1.3.0"
                "1c3njkfzpil03k92q0mij5y1pkhhfr4j3bf0h53bgl2vs85lsjqc"))

(define rust-autocfg-1.4.0
  (crate-source "autocfg" "1.4.0"
                "09lz3by90d2hphbq56znag9v87gfpd9gb8nr82hll8z6x2nhprdc"))

(define rust-autocfg-1.5.0
  (crate-source "autocfg" "1.5.0"
                "1s77f98id9l4af4alklmzq46f21c980v13z2r1pcxx6bqgw0d1n0"))

(define rust-av1-grain-0.2.4
  (crate-source "av1-grain" "0.2.4"
                "1j409cf560kvhxzjs65ksnzbxz7k6sm5lywizan11ijvm0ngngjg"))

(define rust-av1-grain-0.2.5
  (crate-source "av1-grain" "0.2.5"
                "1y3p43i5xncbny0pfh8kw09am3l3mgyg82ln65r3f434443xpzcc"))

(define rust-average-0.16.0
  (crate-source "average" "0.16.0"
                "14d3nwb0b196pdkjsf76gjzw8aql92bphw92ffxlsr0j37kcrakm"))

(define rust-avif-serialize-0.8.6
  (crate-source "avif-serialize" "0.8.6"
                "0gx2yqlh0j5063vk70hv0f1yq40lm6kvc45qx2gm3x1iz30gpj27"))

(define rust-aws-lc-rs-1.15.3
  (crate-source "aws-lc-rs" "1.15.3"
                "11lxrk8dbw5p3pbsn8w1n8kpmsz9krinlz47p7z9q9b7mciyfk78"))

(define rust-aws-lc-sys-0.36.0
  ;; TODO: Check bundled sources.
  (crate-source "aws-lc-sys" "0.36.0"
                "1n2aiw79cdhrnr7z70rk7i6xv63pgabii2scppa3n4b3wgn45923"))

(define rust-aws-sign-v4-0.3.0
  (crate-source "aws-sign-v4" "0.3.0"
                "1my2w0bhba4f39k9x6s5ik210lzh5cay9admxx72mvs8cv2v2dba"))

(define rust-axum-0.8.8
  (crate-source "axum" "0.8.8"
                "1f4p0m04mgwpn8b40i9r5mgqxk6w11sv4yri6xfqk305nhyayllb"))

(define rust-axum-core-0.5.6
  (crate-source "axum-core" "0.5.6"
                "1lcjhxysnbc64rh21ag9m9fpiryd1iwcdh9mwxz1yadiswqqziq8"))

(define rust-axum-server-0.8.0
  (crate-source "axum-server" "0.8.0"
                "1z1bsb3dhk3xq2wnx4ilrlwkcig6a5qi4f1b96ws10nrhcb37pxi"))

(define rust-backtrace-0.3.67
  (crate-source "backtrace" "0.3.67"
                "1jk48laqafvbyc0mn1v0didk307qc0zji3z5jcq2lpqqdmnkfg93"))

(define rust-backtrace-0.3.69
  (crate-source "backtrace" "0.3.69"
                "0dsq23dhw4pfndkx2nsa1ml2g31idm7ss7ljxp8d57avygivg290"))

(define rust-backtrace-0.3.71
  (crate-source "backtrace" "0.3.71"
                "17bgd7pbjb9gc8q47qwsg2lmy9i62x3bsjmmnjrwh5z8s805ic16"))

(define rust-backtrace-0.3.73
  (crate-source "backtrace" "0.3.73"
                "02iffg2pkg5nc36pgml8il7f77s138hhjw9f9l56v5zqlilk5hjw"))

(define rust-backtrace-0.3.74
  (crate-source "backtrace" "0.3.74"
                "06pfif7nwx66qf2zaanc2fcq7m64i91ki9imw9xd3bnz5hrwp0ld"))

(define rust-backtrace-0.3.76
  (crate-source "backtrace" "0.3.76"
                "1mibx75x4jf6wz7qjifynld3hpw3vq6sy3d3c9y5s88sg59ihlxv"))

(define rust-base16ct-0.2.0
  (crate-source "base16ct" "0.2.0"
                "1kylrjhdzk7qpknrvlphw8ywdnvvg39dizw9622w3wk5xba04zsc"))

(define rust-base64-0.13.1
  (crate-source "base64" "0.13.1"
                "1s494mqmzjb766fy1kqlccgfg2sdcjb6hzbvzqv2jw65fdi5h6wy"))

(define rust-base64-0.21.0
  (crate-source "base64" "0.21.0"
                "0sidjip5b33sr6w7kasfj9qxpbda41nw0x4gjjk55g55a6mdv954"))

(define rust-base64-0.21.3
  (crate-source "base64" "0.21.3"
                "0lvf1ishhckkjwiamhqr3iwy5ddrzgvgqfkblwkcaxrxqvxwwka1"))

(define rust-base64-0.21.7
  (crate-source "base64" "0.21.7"
                "0rw52yvsk75kar9wgqfwgb414kvil1gn7mqkrhn9zf1537mpsacx"))

(define rust-base64-0.22.0
  (crate-source "base64" "0.22.0"
                "0lbsphibsb0lnyswqr8mjqw6am7zh780yh62ldbbwl8lxipqcxcl"))

(define rust-base64-0.22.1
  (crate-source "base64" "0.22.1"
                "1imqzgh7bxcikp5vx3shqvw9j09g9ly0xr0jma0q66i52r7jbcvj"))

(define rust-base64ct-1.6.0
  (crate-source "base64ct" "1.6.0"
                "0nvdba4jb8aikv60az40x2w1y96sjdq8z3yp09rwzmkhiwv1lg4c"))

(define rust-beef-0.5.2
  (crate-source "beef" "0.5.2"
                "1c95lbnhld96iwwbyh5kzykbpysq0fnjfhwxa1mhap5qxgrl30is"))

(define rust-bet-1.0.4
  (crate-source "bet" "1.0.4"
                "1dhay8ypq3gy7hbzkfn4633r5sn1q4cc1yfzzvngcz9pk1dcscsa"))

(define rust-bet-1.1.0
  (crate-source "bet" "1.1.0"
                "01ch1ks2dwv0pxpw58m47pk4zil5c58khr4y9zjjh648bzj60s53"))

(define rust-better-panic-0.3.0
  (crate-source "better-panic" "0.3.0"
                "0djh7qs39z0mbkzxs4nrc9ngnyjpsxq67lqfv75q91i63b8y3abg"))

(define rust-bincode-1.3.3
  (crate-source "bincode" "1.3.3"
                "1bfw3mnwzx5g1465kiqllp5n4r10qrqy88kdlp3jfwnq2ya5xx5i"))

(define rust-bincode-2.0.1
  (crate-source "bincode" "2.0.1"
                "0h5pxp3dqkigjwy926a03sl69n9wv7aq4142a20kw9lhn3bzbsin"))

(define rust-bincode-derive-2.0.1
  (crate-source "bincode_derive" "2.0.1"
                "029wmh26hq3hhs1gq629y0frn2pkl7ld061rk23fji8g8jd715dz"))

(define rust-bindgen-0.65.1
  (crate-source "bindgen" "0.65.1"
                "1i9wci1h3xnk8hi7cf06capgifnmpk9dd59zqznh6jcsdx37ppyg"))

(define rust-bindgen-0.70.1
  (crate-source "bindgen" "0.70.1"
                "0vyf0jp6apcy9kjyz4s8vldj0xqycnbzb6zv3skkwiqdi3nqz7gl"))

(define rust-bindgen-0.72.1
  (crate-source "bindgen" "0.72.1"
                "15bq73y3wd3x3vxh3z3g72hy08zs8rxg1f0i1xsrrd6g16spcdwr"))

(define rust-bit-field-0.10.3
  (crate-source "bit_field" "0.10.3"
                "1ikhbph4ap4w692c33r8bbv6yd2qxm1q3f64845grp1s6b3l0jqy"))

(define rust-bit-set-0.5.3
  (crate-source "bit-set" "0.5.3"
                "1wcm9vxi00ma4rcxkl3pzzjli6ihrpn9cfdi0c5b4cvga2mxs007"))

(define rust-bit-set-0.8.0
  (crate-source "bit-set" "0.8.0"
                "18riaa10s6n59n39vix0cr7l2dgwdhcpbcm97x1xbyfp1q47x008"))

(define rust-bit-vec-0.6.3
  (crate-source "bit-vec" "0.6.3"
                "1ywqjnv60cdh1slhz67psnp422md6jdliji6alq0gmly2xm9p7rl"))

(define rust-bit-vec-0.8.0
  (crate-source "bit-vec" "0.8.0"
                "1xxa1s2cj291r7k1whbxq840jxvmdsq9xgh7bvrxl46m80fllxjy"))

(define rust-bitfield-0.14.0
  (crate-source "bitfield" "0.14.0"
                "1b26k9acwss4qvrl60lf9n83l17d4hj47n5rmpd3iigf9j9n0zid"))

(define rust-bitflags-1.3.2
  (crate-source "bitflags" "1.3.2"
                "12ki6w8gn1ldq7yz9y680llwk5gmrhrzszaa17g1sbrw2r2qvwxy"))

(define rust-bitflags-2.10.0
  (crate-source "bitflags" "2.10.0"
                "1lqxwc3625lcjrjm5vygban9v8a6dlxisp1aqylibiaw52si4bl1"))

(define rust-bitflags-2.4.0
  (crate-source "bitflags" "2.4.0"
                "0dc6xa7flfl59makmhixjcrslwlvdxxwrgxbr8p7bkvz53k2ls5l"))

(define rust-bitflags-2.4.2
  (crate-source "bitflags" "2.4.2"
                "1pqd142hyqlzr7p9djxq2ff0jx07a2sb2xp9lhw69cbf80s0jmzd"))

(define rust-bitflags-2.5.0
  (crate-source "bitflags" "2.5.0"
                "1h91vdx1il069vdiiissj8ymzj130rbiic0dbs77yxjgjim9sjyg"))

(define rust-bitflags-2.6.0
  (crate-source "bitflags" "2.6.0"
                "1pkidwzn3hnxlsl8zizh0bncgbjnw7c41cx7bby26ncbzmiznj5h"))

(define rust-bitflags-2.7.0
  (crate-source "bitflags" "2.7.0"
                "1gp1bjw29d12ij9nx0v02nc78s9d04zmyrwzspn4blyncwmg9qqv"))

(define rust-bitflags-2.8.0
  (crate-source "bitflags" "2.8.0"
                "0dixc6168i98652jxf0z9nbyn0zcis3g6hi6qdr7z5dbhcygas4g"))

(define rust-bitflags-2.9.0
  (crate-source "bitflags" "2.9.0"
                "1gb5w7pxnmx8l2bjz1i6rkbwbm2167k294rhy6cl1y3vbc8i90jw"))

(define rust-bitflags-2.9.1
  (crate-source "bitflags" "2.9.1"
                "0rz9rpp5wywwqb3mxfkywh4drmzci2fch780q7lifbf6bsc5d3hv"))

(define rust-bitflags-2.9.2
  (crate-source "bitflags" "2.9.2"
                "0adahzd1i2kv86k0vzkaxdcw9zjm124x9698yp7qgmiimd2varba"))

(define rust-bitflags-2.9.4
  (crate-source "bitflags" "2.9.4"
                "157kkcv8s7vk6d17dar1pa5cqcz4c8pdrn16wm1ld7jnr86d2q92"))

(define rust-bitstream-io-2.6.0
  (crate-source "bitstream-io" "2.6.0"
                "1cli390l1dhp9skygyjjnqvczp36b7f31mkx9ry3dg26330cv6b0"))

(define rust-bitvec-1.0.1
  (crate-source "bitvec" "1.0.1"
                "173ydyj2q5vwj88k6xgjnfsshs4x9wbvjjv7sm0h36r34hn87hhv"))

(define rust-blake2-0.10.6
  (crate-source "blake2" "0.10.6"
                "1zlf7w7gql12v61d9jcbbswa3dw8qxsjglylsiljp9f9b3a2ll26"))

(define rust-blake3-1.8.2
  (crate-source "blake3" "1.8.2"
                "1854x65zmjh9w9cfhyyyg0wmm2k5d87l13l4m7y40ajbkslam21q"))

(define rust-block-0.1.6
  (crate-source "block" "0.1.6"
                "16k9jgll25pzsq14f244q22cdv0zb4bqacldg3kx6h89d7piz30d"))

(define rust-block-buffer-0.10.4
  (crate-source "block-buffer" "0.10.4"
                "0w9sa2ypmrsqqvc20nhwr75wbb5cjr4kkyhpjm1z1lv2kdicfy1h"))

(define rust-block-padding-0.3.3
  (crate-source "block-padding" "0.3.3"
                "14wdad0r1qk5gmszxqd8cky6vx8qg7c153jv981mixzrpzmlz2d8"))

(define rust-block2-0.5.1
  (crate-source "block2" "0.5.1"
                "0pyiha5his2grzqr3mynmq244laql2j20992i59asp0gy7mjw4rc"))

(define rust-block2-0.6.2
  (crate-source "block2" "0.6.2"
                "1xcfllzx6c3jc554nmb5qy6xmlkl6l6j5ib4wd11800n0n3rvsyd"))

(define rust-blocking-1.3.1
  (crate-source "blocking" "1.3.1"
                "0rddcndhxvnxr2wsy0ja2s7gpkwjrr863v1307y9c5l0iwf1l8vp"))

(define rust-blocking-1.6.1
  (crate-source "blocking" "1.6.1"
                "1si99l8zp7c4zq87y35ayjgc5c9b60jb8h0k14zfcs679z2l2gvh"))

(define rust-blocking-1.6.2
  (crate-source "blocking" "1.6.2"
                "08bz3f9agqlp3102snkvsll6wc9ag7x5m1xy45ak2rv9pq18sgz8"))

(define rust-blowfish-0.9.1
  (crate-source "blowfish" "0.9.1"
                "1mw7bvj3bg5w8vh9xw9xawqh7ixk2xwsxkj34ph96b9b1z6y44p4"))

(define rust-bluer-0.16.1
  (crate-source "bluer" "0.16.1"
                "0wz3nqxfbkjzxzn6r8p4z7ids36fk254fpjb1x5gk6v6c29rdmxf"))

(define rust-bluer-0.17.4
  (crate-source "bluer" "0.17.4"
                "00d9n6gx05kr8yr08nyzcpsmay41961sdvmhr2an86b0bhpi2s5g"))

(define rust-borrow-or-share-0.2.2
  (crate-source "borrow-or-share" "0.2.2"
                "0ciski5i69a8mx6f0fh901hn73fii3g39lpl8k3xgi88651b9siy"))

(define rust-borrow-or-share-0.2.4
  (crate-source "borrow-or-share" "0.2.4"
                "0v0nygw2hbzpbzj7lgk5fnvzxssnh1asnm98ii652x0qmm73c2yw"))

(define rust-borsh-1.5.7
  (crate-source "borsh" "1.5.7"
                "1kikljm5yr3l9qsw5xvdccragxj4445s4s3fqsgy6hmmipwld1md"))

(define rust-borsh-1.6.0
  (crate-source "borsh" "1.6.0"
                "0vvsmxiksyw5fq3ap7i7f9hpdrr9hyb8vj7pxwg70dqlgjvmmnni"))

(define rust-borsh-derive-1.6.0
  (crate-source "borsh-derive" "1.6.0"
                "0b2ybhmhc60hdn84sgn02kv91xidl3bdfdprk120rb3am9bci1h6"))

(define rust-bounded-static-0.8.0
  (crate-source "bounded-static" "0.8.0"
                "08ck16yyfpmhbzwhik8nif6n7bgr6gkfnpjxzg53pd29m8yr1sqb"))

(define rust-bounded-static-derive-0.8.0
  (crate-source "bounded-static-derive" "0.8.0"
                "1yhv26kijlv27br1j67rj2ilck0kgbj7zybm96hpmmg54w70bbz0"))

(define rust-brotli-8.0.2
  (crate-source "brotli" "8.0.2"
                "0q25r00z3gm5wzvv4vfxvlx5zjb8i4jwyznrvdcp7abs7ihbkn2b"))

(define rust-brotli-decompressor-5.0.0
  (crate-source "brotli-decompressor" "5.0.0"
                "00yyswj1rj20ma4wr4wcci4r9ywlgvxa87nqsv5rik5y588vhjw7"))

(define rust-bstr-1.11.3
  (crate-source "bstr" "1.11.3"
                "1q3g2wmrvclgx7lk2p6mpzhqxzx41hyg962gkmlyxql1liar26jk"))

(define rust-bstr-1.12.0
  (crate-source "bstr" "1.12.0"
                "195i0gd7r7jg7a8spkmw08492n7rmiabcvz880xn2z8dkp8i6h93"))

(define rust-bstr-1.12.1
  (crate-source "bstr" "1.12.1"
                "1arc1v7h5l86vd6z76z3xykjzldqd5icldn7j9d3p7z6x0d4w133"))

(define rust-bstr-1.8.0
  (crate-source "bstr" "1.8.0"
                "0313sqdf0a40vhpnrlkf54zhr76rmlyxzhx00sq8822shfl36bsl"))

(define rust-buffer-redux-1.0.2
  (crate-source "buffer-redux" "1.0.2"
                "1waq39blrj7j6qp1sp2fvplwmq10yhks7fgbsdy8kxdrqn3wz2jf"))

(define rust-build-rs-0.1.2
  (crate-source "build-rs" "0.1.2"
                "1lkxq4q9l3gg5jdsfca58rjq5x8647c0v2hv23czi6cccriqf2xh"))

(define rust-built-0.7.7
  (crate-source "built" "0.7.7"
                "0ywn0m11xm80pg6zrzq3sdj3vmzg3qs6baqnvfmkd377ly8n3van"))

(define rust-bumpalo-3.12.0
  (crate-source "bumpalo" "3.12.0"
                "0damxqdgqqzp3zyfwvbrg5hzx39kqgxnxl3yyq3kk4ald0jiw9hd"))

(define rust-bumpalo-3.16.0
  (crate-source "bumpalo" "3.16.0"
                "0b015qb4knwanbdlp1x48pkb4pm57b8gidbhhhxr900q2wb6fabr"))

(define rust-bumpalo-3.17.0
  (crate-source "bumpalo" "3.17.0"
                "1gxxsn2fsjmv03g8p3m749mczv2k4m8xspifs5l7bcx0vx3gna0n"))

(define rust-bumpalo-3.19.0
  (crate-source "bumpalo" "3.19.0"
                "0hsdndvcpqbjb85ghrhska2qxvp9i75q2vb70hma9fxqawdy9ia6"))

(define rust-bumpalo-3.19.1
  (crate-source "bumpalo" "3.19.1"
                "044555i277xcinmqs7nnv8n5y4fqfi4l4lp1mp3i30vsidrxrnax"))

(define rust-by-address-1.2.1
  (crate-source "by_address" "1.2.1"
                "01idmag3lcwnnqrnnyik2gmbrr34drsi97q15ihvcbbidf2kryk4"))

(define rust-byte-unit-5.2.0
  (crate-source "byte-unit" "5.2.0"
                "13f5yivhwpw7gv39nkq5wxj2yps5wvm4zifg3drbh7wnwaj4fvcc"))

(define rust-bytecheck-0.6.12
  (crate-source "bytecheck" "0.6.12"
                "1hmipv4yyxgbamcbw5r65wagv9khs033v9483s9kri9sw9ycbk93"))

(define rust-bytecheck-derive-0.6.12
  (crate-source "bytecheck_derive" "0.6.12"
                "0ng6230brd0hvqpbgcx83inn74mdv3abwn95x515bndwkz90dd1x"))

(define rust-bytecount-0.6.9
  (crate-source "bytecount" "0.6.9"
                "0pinq0n8zza8qr2lyc3yf17k963129kdbf0bwnmvdk1bpvh14n0p"))

(define rust-bytemuck-1.21.0
  (crate-source "bytemuck" "1.21.0"
                "18wj81x9xhqcd6985r8qxmbik6szjfjfj62q3xklw8h2p3x7srgg"))

(define rust-bytemuck-1.23.2
  (crate-source "bytemuck" "1.23.2"
                "0xs637lsr9p73ackjkmbjw80dp1dfdw0ydhdk0gzjcnzpkpfm59r"))

(define rust-bytemuck-1.24.0
  (crate-source "bytemuck" "1.24.0"
                "1x65wc9kwf0dfnmglkl8r46d29pfl7yilll5wh9bcf0g6a0gbg8z"))

(define rust-bytemuck-derive-1.10.1
  (crate-source "bytemuck_derive" "1.10.1"
                "0a9dczfzn2c1lgg7afhqrh2drmg34w49hxhipni6pjri49blw5ag"))

(define rust-bytemuck-derive-1.10.2
  (crate-source "bytemuck_derive" "1.10.2"
                "1zvmjmw1sdmx9znzm4dpbb2yvz9vyim8w6gp4z256l46qqdvvazr"))

(define rust-bytemuck-derive-1.8.1
  (crate-source "bytemuck_derive" "1.8.1"
                "0ykwbnpm9y8hssp8kiaws2s4hljv4cl85mwgp1m67fzpnj9n59rz"))

(define rust-byteorder-1.4.3
  (crate-source "byteorder" "1.4.3"
                "0456lv9xi1a5bcm32arknf33ikv76p3fr9yzki4lb2897p2qkh8l"))

(define rust-byteorder-1.5.0
  (crate-source "byteorder" "1.5.0"
                "0jzncxyf404mwqdbspihyzpkndfgda450l0893pz5xj685cg5l0z"))

(define rust-byteorder-lite-0.1.0
  (crate-source "byteorder-lite" "0.1.0"
                "15alafmz4b9az56z6x7glcbcb6a8bfgyd109qc3bvx07zx4fj7wg"))

(define rust-bytes-0.3.0
  (crate-source "bytes" "0.3.0"
                "09vcp9kh12pva2xn2ir79k90v1a0id8f4sdv1abn5ifw2bqsyaf1"))

(define rust-bytes-1.10.0
  (crate-source "bytes" "1.10.0"
                "1ybcmdrlxrsrn7lnl0xrjg10j7zb4r01jjs5b2sqhrcwh62aq7gn"))

(define rust-bytes-1.10.1
  (crate-source "bytes" "1.10.1"
                "0smd4wi2yrhp5pmq571yiaqx84bjqlm1ixqhnvfwzzc6pqkn26yp"))

(define rust-bytes-1.11.0
  (crate-source "bytes" "1.11.0"
                "1cww1ybcvisyj8pbzl4m36bni2jaz0narhczp1348gqbvkxh8lmk"))

(define rust-bytes-1.4.0
  (crate-source "bytes" "1.4.0"
                "1gkh3fk4fm9xv5znlib723h5md5sxsvbd5113sbxff6g1lmgvcl9"))

(define rust-bytes-1.5.0
  (crate-source "bytes" "1.5.0"
                "08w2i8ac912l8vlvkv3q51cd4gr09pwlg3sjsjffcizlrb0i5gd2"))

(define rust-bytes-1.6.0
  (crate-source "bytes" "1.6.0"
                "1jf2awc1fywpk15m6pxay3wqcg65ararg9xi4b08vnszwiyy2kai"))

(define rust-bytes-1.7.1
  (crate-source "bytes" "1.7.1"
                "0l5sf69avjxcw41cznyzxsnymwmkpmk08q0sm7fgicvvn0ysa643"))

(define rust-bytes-1.9.0
  (crate-source "bytes" "1.9.0"
                "16ykzx24v1x4f42v2lxyvlczqhdfji3v7r4ghwckpwijzvb1hn9j"))

(define rust-bytesize-2.3.1
  (crate-source "bytesize" "2.3.1"
                "18vjp7wfik0xi54l3z2314s7zxjyywa4vvwhilawnas2nbkixnbb"))

(define rust-bytestring-1.5.0
  (crate-source "bytestring" "1.5.0"
                "128j4zlv63dr0z1lfhsr7bki5c687kgdikh1six7wqgnnm1l6fqi"))

(define rust-cairo-rs-0.18.5
  (crate-source "cairo-rs" "0.18.5"
                "1qjfkcq3mrh3p01nnn71dy3kn99g21xx3j8xcdvzn8ll2pq6x8lc"))

(define rust-cairo-rs-0.21.2
  (crate-source "cairo-rs" "0.21.2"
                "1cc1xyhcch1p3s85b2zc732rz3qz11wpcf33wdq8hr6syi6kbr6z"))

(define rust-cairo-sys-rs-0.18.2
  (crate-source "cairo-sys-rs" "0.18.2"
                "0lfsxl7ylw3phbnwmz3k58j1gnqi6kc2hdc7g3bb7f4hwnl9yp38"))

(define rust-cairo-sys-rs-0.21.2
  (crate-source "cairo-sys-rs" "0.21.2"
                "1y03q419jfks67ycdy1q8ik2bahi6l092xg09n7fn0vi1hqc7mj7"))

(define rust-calcard-0.2.0
  (crate-source "calcard" "0.2.0"
                "0mdlgci96fghiw11qc5dsjcz6gcr9xcmck72nknglk40kwwk61xw"))

(define rust-calloop-0.12.3
  (crate-source "calloop" "0.12.3"
                "1gxd20zpys0q4v9fg3nrkyhrnjgpkgriidgfm5aqraar9njbal3v"))

(define rust-calloop-0.13.0
  (crate-source "calloop" "0.13.0"
                "1v5zgidnhsyml403rzr7vm99f8q6r5bxq5gxyiqkr8lcapwa57dr"))

(define rust-calloop-0.14.3
  (crate-source "calloop" "0.14.3"
                "17ih3c840cqksv9ms7i2ynnkiabpvqvpxakbr3922imxd09nx7yb"))

(define rust-calloop-wayland-source-0.2.0
  (crate-source "calloop-wayland-source" "0.2.0"
                "00lgxlqnkd150ylfn3wi4464ibif1qwbp38shj1aszvc8ywsj3hg"))

(define rust-calloop-wayland-source-0.3.0
  (crate-source "calloop-wayland-source" "0.3.0"
                "086x5mq16prrcwd9k6bw9an0sp8bj9l5daz4ziz5z4snf2c6m9lm"))

(define rust-calloop-wayland-source-0.4.1
  (crate-source "calloop-wayland-source" "0.4.1"
                "1yi1c23naqhd8m94q3v366s4cak8l50zy7ldrkqfn0hajkqgr3hk"))

(define rust-camellia-0.1.0
  (crate-source "camellia" "0.1.0"
                "0c6f61rf0gzq7x9d2qmp0330pb397aldwdpmwqybbwly9rby4r1j"))

(define rust-camino-1.1.11
  (crate-source "camino" "1.1.11"
                "1h2150limbipfx5w59cw797nsgwjxn5mjpf33gvpc35hjfdal1sx"))

(define rust-camino-1.1.6
  (crate-source "camino" "1.1.6"
                "171vzfyrm2jmajd70q1m774297y028kadgm7cfw4kxc8lfsr57n5"))

(define rust-camino-1.2.1
  (crate-source "camino" "1.2.1"
                "0296bpl9smby45fc2583cgb7ybdiqpqcjh4k2f47g5ic5fzmjsi7"))

(define rust-cassowary-0.3.0
  (crate-source "cassowary" "0.3.0"
                "0lvanj0gsk6pc1chqrh4k5k0vi1rfbgzmsk46dwy3nmrqyw711nz"))

(define rust-cast-0.3.0
  (crate-source "cast" "0.3.0"
                "1dbyngbyz2qkk0jn2sxil8vrz3rnpcj142y184p9l4nbl9radcip"))

(define rust-cast5-0.11.1
  (crate-source "cast5" "0.11.1"
                "04crg8dj6lxbp3lmdc3filsahxcyvccvhm0gx40g1k5i7mkpvc16"))

(define rust-castaway-0.2.3
  (crate-source "castaway" "0.2.3"
                "1mf0wypwnkpa1hi0058vp8g7bjh2qraip2qv7dmak7mg1azfkfha"))

(define rust-castaway-0.2.4
  (crate-source "castaway" "0.2.4"
                "0nn5his5f8q20nkyg1nwb40xc19a08yaj4y76a8q2y3mdsmm3ify"))

(define rust-cbc-0.1.2
  (crate-source "cbc" "0.1.2"
                "19l9y9ccv1ffg6876hshd123f2f8v7zbkc4nkckqycxf8fajmd96"))

(define rust-cc-1.0.79
  (crate-source "cc" "1.0.79"
                "07x93b8zbf3xc2dggdd460xlk1wg8lxm6yflwddxj8b15030klsh"))

(define rust-cc-1.0.83
  (crate-source "cc" "1.0.83"
                "1l643zidlb5iy1dskc5ggqs4wqa29a02f44piczqc8zcnsq4y5zi"))

(define rust-cc-1.0.99
  (crate-source "cc" "1.0.99"
                "15gnhjiyl323b6lmm05sn8rasyckjqf4n2w7gym4y4j4zmki1icn"))

(define rust-cc-1.1.15
  (crate-source "cc" "1.1.15"
                "1rn62w58ba1ylqlp3saj4n0vh1h40ii1r83xr06p80r9m9ss5djp"))

(define rust-cc-1.2.15
  (crate-source "cc" "1.2.15"
                "1bq1c3qbarhx3z10bfpk8df2kq2akx7k0v68sm1z8xx5xrcy4dn7"))

(define rust-cc-1.2.16
  (crate-source "cc" "1.2.16"
                "131bhgafc1i86vvjipkj0kwzz0hlpwrkl8mdbmzyq2g69calqwdy"))

(define rust-cc-1.2.18
  (crate-source "cc" "1.2.18"
                "0p6d2pfyrjgqpf2w399wzj4hmyffj6g0gyzg3pdy6xl3gmhlcl2j"))

(define rust-cc-1.2.32
  (crate-source "cc" "1.2.32"
                "0vi9np1fac712q5dpa2gb3g8lwr7bl6ik5cwdmg4sm4wgrcyali3"))

(define rust-cc-1.2.33
  (crate-source "cc" "1.2.33"
                "0pwv1ql0gpvacwdn44643adr0s0q8p575pbp4xz5mfi26a0giq1y"))

(define rust-cc-1.2.39
  (crate-source "cc" "1.2.39"
                "0py3546wz3k5qi6pbfz80jvg0g3qgzr21c7a1p5wjvscjm4l6dg1"))

(define rust-cc-1.2.40
  (crate-source "cc" "1.2.40"
                "1ywr07jr2rj1h7gkgkc2c1bn39wchpfnrm39sjm7dzdiyj95vl71"))

(define rust-cc-1.2.44
  (crate-source "cc" "1.2.44"
                "1wxhvq65g8j0acx5pk9ckllnyhf940z4k0n35l916gdymb3illip"))

(define rust-cc-1.2.49
  (crate-source "cc" "1.2.49"
                "05929ra8a2q81w45f932nr4blifnxkpr8i7lmcba28bm0c4k0n4h"))

(define rust-cc-1.2.50
  (crate-source "cc" "1.2.50"
                "0g1q2k30bwnym5hlhk2y3k08hhrklgn68gr61b63f73s49ixal4z"))

(define rust-cc-1.2.52
  (crate-source "cc" "1.2.52"
                "18xdg9jj7yc6zxcpc1cr8xa1yhim03za8mh9qqv2nh0jznp34jfd"))

(define rust-cc-1.2.53
  (crate-source "cc" "1.2.53"
                "0cjrx2nzlz8l93p4sfymjgsv0jicvfphd6hyhk5gyxbi2z72ypbm"))

(define rust-cc-1.2.6
  (crate-source "cc" "1.2.6"
                "0cx32v9pcslavf8y10sb3y883v7377mw48q3dpw5b1cgidibnvcd"))

(define rust-cc-1.2.8
  (crate-source "cc" "1.2.8"
                "0nm5z96vzzq4gbivfbmwm10lcl4qlyxwczpfqrrwfi6y3zlzc35d"))

(define rust-cesu8-1.1.0
  (crate-source "cesu8" "1.1.0"
                "0g6q58wa7khxrxcxgnqyi9s1z2cjywwwd3hzr5c55wskhx6s0hvd"))

(define rust-cexpr-0.6.0
  (crate-source "cexpr" "0.6.0"
                "0rl77bwhs5p979ih4r0202cn5jrfsrbgrksp40lkfz5vk1x3ib3g"))

(define rust-cfb-mode-0.8.2
  (crate-source "cfb-mode" "0.8.2"
                "0c6kd34jk4p52vr0qgn9slj6zdgmc42gfcqr6mqhmy37g138v2vk"))

(define rust-cfg-aliases-0.1.1
  (crate-source "cfg_aliases" "0.1.1"
                "17p821nc6jm830vzl2lmwz60g3a30hcm33nk6l257i1rjdqw85px"))

(define rust-cfg-aliases-0.2.1
  (crate-source "cfg_aliases" "0.2.1"
                "092pxdc1dbgjb6qvh83gk56rkic2n2ybm4yvy76cgynmzi3zwfk1"))

(define rust-cfg-expr-0.14.0
  (crate-source "cfg-expr" "0.14.0"
                "1im4f8zrd0gaflv4sxjr2vdn2z4600lyhz32bjc342llc5a2anx3"))

(define rust-cfg-expr-0.15.7
  (crate-source "cfg-expr" "0.15.7"
                "07cny7rg58mlzmzw2b1x5b6ib1za9647gklksnlzv9m9cj5qcl7s"))

(define rust-cfg-expr-0.15.8
  (crate-source "cfg-expr" "0.15.8"
                "00lgf717pmf5qd2qsxxzs815v6baqg38d6m5i6wlh235p14asryh"))

(define rust-cfg-expr-0.17.2
  (crate-source "cfg-expr" "0.17.2"
                "12a7zr6ff4i6mfwcv711dll0w5pr3dw1lvkaf4c4a66i1gjacjwd"))

(define rust-cfg-if-0.1.10
  (crate-source "cfg-if" "0.1.10"
                "08h80ihs74jcyp24cd75wwabygbbdgl05k6p5dmq8akbr78vv1a7"))

(define rust-cfg-if-1.0.0
  (crate-source "cfg-if" "1.0.0"
                "1za0vb97n4brpzpv8lsbnzmq5r8f2b0cpqqr0sy8h5bn751xxwds"))

(define rust-cfg-if-1.0.1
  (crate-source "cfg-if" "1.0.1"
                "0s0jr5j797q1vqjcd41l0v5izlmlqm7lxy512b418xz5r65mfmcm"))

(define rust-cfg-if-1.0.3
  (crate-source "cfg-if" "1.0.3"
                "1afg7146gbxjvkbjx7i5sdrpqp9q5akmk9004fr8rsm90jf2il9g"))

(define rust-cfg-if-1.0.4
  (crate-source "cfg-if" "1.0.4"
                "008q28ajc546z5p2hcwdnckmg0hia7rnx52fni04bwqkzyrghc4k"))

(define rust-char-reader-0.1.1
  (crate-source "char_reader" "0.1.1"
                "0fpdhx6061a92fj3kw11rwv2zkdlx8z59gbkq7baf762vqi9p99p"))

(define rust-chrono-0.4.38
  (crate-source "chrono" "0.4.38"
                "009l8vc5p8750vn02z30mblg4pv2qhkbfizhfwmzc6vpy5nr67x2"))

(define rust-chrono-0.4.39
  (crate-source "chrono" "0.4.39"
                "09g8nf409lb184kl9j4s85k0kn8wzgjkp5ls9zid50b886fwqdky"))

(define rust-chrono-0.4.40
  (crate-source "chrono" "0.4.40"
                "0z334kqnvq5zx6xsq1k6zk8g9z14fgk2w3vkn4n13pvi3mhn8y8s"))

(define rust-chrono-0.4.42
  (crate-source "chrono" "0.4.42"
                "1lp8iz9js9jwxw0sj8yi59v54lgvwdvm49b9wch77f25sfym4l0l"))

(define rust-chrono-0.4.43
  (crate-source "chrono" "0.4.43"
                "06312amlyys4kkjazl13mbxw0j2f7zxygzjkr1yk7s2sn57p9i7s"))

(define rust-chrono-humanize-0.2.3
  (crate-source "chrono-humanize" "0.2.3"
                "0fq25fcdqd7s39dx81hq123210q4lpcbjdz82jl2fy6jnkk2g5kr"))

(define rust-chrono-tz-0.10.4
  (crate-source "chrono-tz" "0.10.4"
                "1hr6rmdvqwgk748g2f69mnk97fzhdkfzaczvdn0wz4pdjy2rl4x6"))

(define rust-chumsky-1.0.0-alpha.7
  (crate-source "chumsky" "1.0.0-alpha.7"
                "07qgyayw2vyrgfzfp4b0qsiwvadvacnm99kcqnfpi1kgk1v05f67"))

(define rust-ciborium-0.2.2
  (crate-source "ciborium" "0.2.2"
                "03hgfw4674im1pdqblcp77m7rc8x2v828si5570ga5q9dzyrzrj2"))

(define rust-ciborium-io-0.2.2
  (crate-source "ciborium-io" "0.2.2"
                "0my7s5g24hvp1rs1zd1cxapz94inrvqpdf1rslrvxj8618gfmbq5"))

(define rust-ciborium-ll-0.2.2
  (crate-source "ciborium-ll" "0.2.2"
                "1n8g4j5rwkfs3rzfi6g1p7ngmz6m5yxsksryzf5k72ll7mjknrjp"))

(define rust-cipher-0.4.4
  (crate-source "cipher" "0.4.4"
                "1b9x9agg67xq5nq879z66ni4l08m6m3hqcshk37d4is4ysd3ngvp"))

(define rust-clang-sys-1.8.1
  ;; TODO: Check bundled sources.
  (crate-source "clang-sys" "1.8.1"
                "1x1r9yqss76z8xwpdanw313ss6fniwc1r7dzb5ycjn0ph53kj0hb"))

(define rust-clap-4.5.13
  (crate-source "clap" "4.5.13"
                "1z5773cyvhb47rxflng7mi7vrkc4zw2f6xg47l7pja1l0l52dfqg"))

(define rust-clap-4.5.23
  (crate-source "clap" "4.5.23"
                "110cf0i9fmkfqzqhi1h8za9y0vnr5rwhy3wmv1p0rcgp5vnffd9i"))

(define rust-clap-4.5.26
  (crate-source "clap" "4.5.26"
                "10v7qvn90calfbhap1c4r249i5c7fbxj09fn3szfz9pkis85xsx8"))

(define rust-clap-4.5.35
  (crate-source "clap" "4.5.35"
                "0i1rnz7mwbhs5qf10r6vmrkplkzm3477khkwz189rha49f9qdanq"))

(define rust-clap-4.5.45
  (crate-source "clap" "4.5.45"
                "0663m85dd0aq1g3mkwz5b8pkjv4f5k2smlz7bagib4iqf15fgh0z"))

(define rust-clap-4.5.48
  (crate-source "clap" "4.5.48"
                "1bjz3d7bavy13ph2a6rm3c9y02ak70b195xakii7h6q2xarln4z2"))

(define rust-clap-4.5.49
  (crate-source "clap" "4.5.49"
                "13sha7m7slskx12nsw6z45chy82xgh0q8lga5j9sklv8za82nlgl"))

(define rust-clap-4.5.51
  (crate-source "clap" "4.5.51"
                "1xg5fa4lcl2pmdhlns1qzw5dpz1yz6hxk6x6j7qra0hf2whxf9jc"))

(define rust-clap-4.5.53
  (crate-source "clap" "4.5.53"
                "1y035lyy5w2xx83q4c3jiy75928ldm1x2bi8ylslkgx12bh41qy9"))

(define rust-clap-4.5.54
  (crate-source "clap" "4.5.54"
                "15737jmai272j6jh4ha4dq4ap14ysx2sa5wsjv6zbkvrrnfzzrn6"))

(define rust-clap-builder-4.5.13
  (crate-source "clap_builder" "4.5.13"
                "16adkdf8jbxpfn4jly54xn1ngwign57vwb5zgly877sflxz7vcb4"))

(define rust-clap-builder-4.5.23
  (crate-source "clap_builder" "4.5.23"
                "0f28rgc09kdgfq1hgg1bb1ydaw243w6dwyw74syz439k6b32yn1h"))

(define rust-clap-builder-4.5.26
  (crate-source "clap_builder" "4.5.26"
                "08f1mzcvi7zjhm7hvz6al4jnv70ccqhwiaq74hihlspwnl0iic4n"))

(define rust-clap-builder-4.5.35
  (crate-source "clap_builder" "4.5.35"
                "1nczcw6cc49ap99nn3v3n0vrv7j74zin34palq6ji586vnrdn514"))

(define rust-clap-builder-4.5.44
  (crate-source "clap_builder" "4.5.44"
                "1a48x3c9q1l7r6wbgy71mq6kfsihpqzxsnbaaamcgwvp88hz9rxk"))

(define rust-clap-builder-4.5.48
  (crate-source "clap_builder" "4.5.48"
                "1jaxnr7ik25r4yxgz657vm8kz62f64qmwxhplmzxz9n0lfpn9fn2"))

(define rust-clap-builder-4.5.49
  (crate-source "clap_builder" "4.5.49"
                "0c37pnanfm3jlh0av6jn36x1wyl8b9lz8gx1gg36drqjma5yj980"))

(define rust-clap-builder-4.5.51
  (crate-source "clap_builder" "4.5.51"
                "06h10byk8dl8b4nypil6aad9kshgfrjyji5ybb8bz0gngc65z0vm"))

(define rust-clap-builder-4.5.53
  (crate-source "clap_builder" "4.5.53"
                "004xasw24a9vvzpiymjkm4khffpyzqwskz7ps8gr1351x89mssyp"))

(define rust-clap-builder-4.5.54
  (crate-source "clap_builder" "4.5.54"
                "001cnl5ccva6z3x5nw3m72zs3bzb650anz1scs7vqhbs5d6wyhps"))

(define rust-clap-cargo-0.18.3
  (crate-source "clap-cargo" "0.18.3"
                "01l070c3a88spywm8drhfzwn3ljp142fqsi1p15pan42bj9m2rck"))

(define rust-clap-complete-4.5.42
  (crate-source "clap_complete" "4.5.42"
                "1l7zqm45vmy2jjsk0h0izgl74wyl39jvbs30wrmlpyjhwxlf99rk"))

(define rust-clap-complete-4.5.47
  (crate-source "clap_complete" "4.5.47"
                "1dkzjgmi0c4jgq4cwvmzbaki9mxanll6d0mw5gwd8ji6x9w56vy0"))

(define rust-clap-complete-4.5.58
  (crate-source "clap_complete" "4.5.58"
                "0jmg0idg96cvx51l35ypia1np3q7sfj5wqxvi7kjs59fmlr0pgvm"))

(define rust-clap-complete-4.5.60
  (crate-source "clap_complete" "4.5.60"
                "0wfrf9rfz8qhb07716i2hmhh1jgai8lmlfx3vy8l4nlwfdbjhq4f"))

(define rust-clap-complete-4.5.61
  (crate-source "clap_complete" "4.5.61"
                "14n9lz4bk308p5dq8hvzcln33gdm5z1xmba90465bqpcw8amjq9r"))

(define rust-clap-complete-nushell-4.5.10
  (crate-source "clap_complete_nushell" "4.5.10"
                "06k4bfrp3rbm0bpqadr4kbb60y8hmcsq8kraagh6fx2bsdpwhnv8"))

(define rust-clap-complete-nushell-4.5.5
  (crate-source "clap_complete_nushell" "4.5.5"
                "12miqxh9g7q37w11bgv55b32s0hdf6avf0lhagzc5psp6icv3a66"))

(define rust-clap-derive-4.5.13
  (crate-source "clap_derive" "4.5.13"
                "1860xq3rbgwsqwcj9rd14cky9iiywwx86j7fvvngdjixbyfka7ah"))

(define rust-clap-derive-4.5.18
  (crate-source "clap_derive" "4.5.18"
                "1ardb26bvcpg72q9myr7yir3a8c83gx7vxk1cccabsd9n73s1ija"))

(define rust-clap-derive-4.5.24
  (crate-source "clap_derive" "4.5.24"
                "131ih3dm76srkbpfx7zfspp9b556zgzj31wqhl0ji2b39lcmbdsl"))

(define rust-clap-derive-4.5.32
  (crate-source "clap_derive" "4.5.32"
                "1mqcag8qapb5yhygg2hi153kzmbf7w5hqp3nl3fvl5cn4yp6l5q9"))

(define rust-clap-derive-4.5.45
  (crate-source "clap_derive" "4.5.45"
                "1xir8wn5d10wpmnzmzjf2k1ib7j5mmzsm6v3yap6qlvx1axk3jql"))

(define rust-clap-derive-4.5.47
  (crate-source "clap_derive" "4.5.47"
                "174z9g13s85la2nmi8gv8ssjwz77im3rqg5isiinw6hg1fp7xzdv"))

(define rust-clap-derive-4.5.49
  (crate-source "clap_derive" "4.5.49"
                "0wbngw649138v3jwx8pm5x9sq0qsml3sh0sfzyrdxcpamy3m82ra"))

(define rust-clap-help-1.5.0
  (crate-source "clap-help" "1.5.0"
                "0nwqnry884f6vky61y8vjkynyya4yggnz7gh7p9rq86s7bknxn59"))

(define rust-clap-lex-0.7.0
  (crate-source "clap_lex" "0.7.0"
                "1kh1sckgq71kay2rrr149pl9gbsrvyccsq6xm5xpnq0cxnyqzk4q"))

(define rust-clap-lex-0.7.4
  (crate-source "clap_lex" "0.7.4"
                "19nwfls5db269js5n822vkc8dw0wjq2h1wf0hgr06ld2g52d2spl"))

(define rust-clap-lex-0.7.5
  (crate-source "clap_lex" "0.7.5"
                "0xb6pjza43irrl99axbhs12pxq4sr8x7xd36p703j57f5i3n2kxr"))

(define rust-clap-lex-0.7.6
  (crate-source "clap_lex" "0.7.6"
                "13cxw9m2rqvplgazgkq2awms0rgf34myc19bz6gywfngi762imx1"))

(define rust-clap-lex-0.7.7
  (crate-source "clap_lex" "0.7.7"
                "0cibsbziyzw2ywar2yh6zllsamhwkblfly565zgi56s3q064prn3"))

(define rust-clap-mangen-0.2.25
  (crate-source "clap_mangen" "0.2.25"
                "1x09zzzkqpqz5g10nc951imz3b2wqgiiifmyd04qshx28anfdgxc"))

(define rust-clap-mangen-0.2.26
  (crate-source "clap_mangen" "0.2.26"
                "0fn1svjqm3znajji679nc2kfwm9lkyl73lzknf4rnkqlkgx44j3j"))

(define rust-clap-mangen-0.2.29
  (crate-source "clap_mangen" "0.2.29"
                "1qkr87xz9mgvh9mqqqqni7fw8dffzihmzwj7rfnxkw1h9g2w7d17"))

(define rust-clap-mangen-0.2.31
  (crate-source "clap_mangen" "0.2.31"
                "00b3r0nym6wvgfas1crmsn3205149ynj2hhnjcwgjv88j8xad7j3"))

(define rust-clap-markdown-0.1.4
  (crate-source "clap-markdown" "0.1.4"
                "1zrcyih2mfgv11cnyzf1smxar8jgf82g5hj12nrzh53f4vk6gg4f"))

(define rust-clap-markdown-0.1.5
  (crate-source "clap-markdown" "0.1.5"
                "0f93ij22sxl1ik0sz25h23n9zc7b0x9pnschnj2lhvd0arwn38nj"))

(define rust-cli-log-2.1.0
  (crate-source "cli-log" "2.1.0"
                "0ip0lir1dc5g0706isx48zhqq4034kjygy2ll1rx8p1rwm3al872"))

(define rust-clipboard-macos-0.1.1
  (crate-source "clipboard_macos" "0.1.1"
                "13zkbmnsl2c98452kvia7vr2ywwl123bj2q81diw78vv0jm4lzwv"))

(define rust-clipboard-wayland-0.2.2
  (crate-source "clipboard_wayland" "0.2.2"
                "1f1prf3qhww5qqbbqbf27ygq103z9r1b678cs4lpg672qimqhgq0"))

(define rust-clipboard-win-4.5.0
  (crate-source "clipboard-win" "4.5.0"
                "0qh3rypkf1lazniq4nr04hxsck0d55rigb5sjvpvgnap4dyc54bi"))

(define rust-clipboard-win-5.4.0
  (crate-source "clipboard-win" "5.4.0"
                "14n87fc0vzbd0wdhqzvcs1lqgafsncplzcanhpik93xhhalfgvqm"))

(define rust-clipboard-win-5.4.1
  (crate-source "clipboard-win" "5.4.1"
                "1m44gqy11rq1ww7jls86ppif98v6kv2wkwk8p17is86zsdq3gq5x"))

(define rust-clipboard-x11-0.4.2
  (crate-source "clipboard_x11" "0.4.2"
                "0703a1naak6iwcy98v2pl04f9521w4ik8qx20jghygh1bs0ylx22"))

(define rust-clru-0.6.2
  (crate-source "clru" "0.6.2"
                "0ngyycxpxif84wpjjn0ixywylk95h5iv8fqycg2zsr3f0rpggl6b"))

(define rust-cmake-0.1.54
  (crate-source "cmake" "0.1.54"
                "1w41ma28yzad9x757s9sfq1wigjs9j902hbzc0nbxpc9vvws7jp7"))

(define rust-cmake-0.1.57
  (crate-source "cmake" "0.1.57"
                "0zgg10qgykig4nxyf7whrqfg7fkk0xfxhiavikmrndvbrm23qi3m"))

(define rust-codespan-reporting-0.11.1
  (crate-source "codespan-reporting" "0.11.1"
                "0vkfay0aqk73d33kh79k1kqxx06ka22894xhqi89crnc6c6jff1m"))

(define rust-codespan-reporting-0.12.0
  (crate-source "codespan-reporting" "0.12.0"
                "108g41xqzhr8fx8hlpy5qzmqq8ylldbj37wndkaqm34yy1d2wvgy"))

(define rust-color-eyre-0.6.3
  (crate-source "color-eyre" "0.6.3"
                "1m9shifr9sdw0drszzyhvaq5jysrsiki44bl7m1gfdzj8rg6y52m"))

(define rust-color-eyre-0.6.5
  (crate-source "color-eyre" "0.6.5"
                "0vgjy8q6c4fin6inh7ik66rkkyjwar3ai8z33vbacckqnkphp4p5"))

(define rust-color-quant-1.1.0
  (crate-source "color_quant" "1.1.0"
                "12q1n427h2bbmmm1mnglr57jaz2dj9apk0plcxw7nwqiai7qjyrx"))

(define rust-color-spantrace-0.2.0
  (crate-source "color-spantrace" "0.2.0"
                "1kldrjm5j3mzz6c84brxshnzm1qbvjglgg7c4z5xrv29jhymp9qv"))

(define rust-color-spantrace-0.2.1
  (crate-source "color-spantrace" "0.2.1"
                "1hkjgaqixrishwiq3lxxy1d4c3mvlv6avcj3ifwy50p3lyrf2syd"))

(define rust-color-spantrace-0.3.0
  (crate-source "color-spantrace" "0.3.0"
                "09xl077fs44yvqajmr52mjbghvkfwg6fnd3jpialnd8kvylqxf5q"))

(define rust-colorchoice-1.0.0
  (crate-source "colorchoice" "1.0.0"
                "1ix7w85kwvyybwi2jdkl3yva2r2bvdcc3ka2grjfzfgrapqimgxc"))

(define rust-colorchoice-1.0.2
  (crate-source "colorchoice" "1.0.2"
                "1h18ph538y8yjmbpaf8li98l0ifms2xmh3rax9666c5qfjfi3zfk"))

(define rust-colorchoice-1.0.3
  (crate-source "colorchoice" "1.0.3"
                "1439m3r3jy3xqck8aa13q658visn71ki76qa93cy55wkmalwlqsv"))

(define rust-colorchoice-1.0.4
  (crate-source "colorchoice" "1.0.4"
                "0x8ymkz1xr77rcj1cfanhf416pc4v681gmkc9dzb3jqja7f62nxh"))

(define rust-colored-2.1.0
  (crate-source "colored" "2.1.0"
                "1f4h9p64snrnz4x432iza15p4diqjcgpmpvhi956d6r1rq61bwnb"))

(define rust-colored-2.2.0
  (crate-source "colored" "2.2.0"
                "0g6s7j2qayjd7i3sivmwiawfdg8c8ldy0g2kl4vwk1yk16hjaxqi"))

(define rust-colpetto-0.7.0
  (crate-source "colpetto" "0.7.0"
                "19qjlpnma08himgw24qqvdlg8hhlvg8z2k1zcb996j4kxyy4y23v"))

(define rust-com-0.6.0
  (crate-source "com" "0.6.0"
                "1xnryd43mqdyq66qlnagwxrcs9iyr0kcbw9f3ddvclvks5zqh5vy"))

(define rust-com-macros-0.6.0
  (crate-source "com_macros" "0.6.0"
                "198k9fqd9rnpv3x6pxav6g636gl6m30iyqx63r4cfs56h0sqhxfk"))

(define rust-com-macros-support-0.6.0
  (crate-source "com_macros_support" "0.6.0"
                "033ix2k6j0930b0gpm77r2zc2d4f5fvpqbbr8ib6sad9hw89m2dd"))

(define rust-combine-4.6.7
  (crate-source "combine" "4.6.7"
                "1z8rh8wp59gf8k23ar010phgs0wgf5i8cx4fg01gwcnzfn5k0nms"))

(define rust-comfy-table-7.1.3
  (crate-source "comfy-table" "7.1.3"
                "1nd4ns4vimypk554vqjww3iq14mdjbaawn5q1jl6w9j3nvknbw94"))

(define rust-comfy-table-7.2.2
  (crate-source "comfy-table" "7.2.2"
                "0ixdw77rly84i5z1mxyw6v8lp1isaawnmgxv5d64n88zrxp5v34m"))

(define rust-compact-str-0.8.1
  (crate-source "compact_str" "0.8.1"
                "0cmgp61hw4fwaakhilwznfgncw2p4wkbvz6dw3i7ibbckh3c8y9v"))

(define rust-compact-str-0.9.0
  (crate-source "compact_str" "0.9.0"
                "0ykhh2scg32lmzxak107pmby6fmnz7qbhsi9i8g9iknfl4ji7nrz"))

(define rust-concurrent-queue-2.2.0
  (crate-source "concurrent-queue" "2.2.0"
                "0z0bnpgcblhrms6gph7x78yplj3qmlr5mvl38v9641zsxiqngv32"))

(define rust-concurrent-queue-2.5.0
  (crate-source "concurrent-queue" "2.5.0"
                "0wrr3mzq2ijdkxwndhf79k952cp4zkz35ray8hvsxl96xrx1k82c"))

(define rust-config-0.15.19
  (crate-source "config" "0.15.19"
                "1mmka6ap5kh253vjgzxzfayz6jz1j7kcl36b0gy6dmxa9hjsh3xk"))

(define rust-confy-0.6.1
  (crate-source "confy" "0.6.1"
                "18795hfza7g0jwbvldl5rammmk3jdaxq5b6w9b1pvw3h130g9ca5"))

(define rust-confy-2.0.0
  (crate-source "confy" "2.0.0"
                "0528xqlri0kwjlzjlyv910l1712m6igyl9qsvfxh5glwg2bw61w8"))

(define rust-console-0.15.11
  (crate-source "console" "0.15.11"
                "1n5gmsjk6isbnw6qss043377kln20lfwlmdk3vswpwpr21dwnk05"))

(define rust-console-0.15.8
  (crate-source "console" "0.15.8"
                "1sz4nl9nz8pkmapqni6py7jxzi7nzqjxzb3ya4kxvmkb0zy867qf"))

(define rust-console-0.16.2
  (crate-source "console" "0.16.2"
                "1i5y6h3myz38jl9p3gglx5vh9c69kxxajsv3jx0pw8i6i555mr03"))

(define rust-console-error-panic-hook-0.1.7
  (crate-source "console_error_panic_hook" "0.1.7"
                "1g5v8s0ndycc10mdn6igy914k645pgpcl8vjpz6nvxkhyirynsm0"))

(define rust-console-log-0.2.2
  (crate-source "console_log" "0.2.2"
                "17s9awnafrz3cb2k4i9brvi8f2bqzcd5lk80ia5qf0c5bvv757z8"))

(define rust-const-oid-0.9.6
  (crate-source "const-oid" "0.9.6"
                "1y0jnqaq7p2wvspnx7qj76m7hjcqpz73qzvr9l2p9n2s51vr6if2"))

(define rust-constant-time-eq-0.3.1
  (crate-source "constant_time_eq" "0.3.1"
                "19nwwczii762pwlsm7bpizgjg8hkg1kqi32b2g4rglijklsbhx3w"))

(define rust-conv-0.3.3
  (crate-source "conv" "0.3.3"
                "168j1npqrif1yqxbgbk0pdrx9shzhs5ylc5a4xw49b6hbxi11zvq"))

(define rust-convert-case-0.10.0
  (crate-source "convert_case" "0.10.0"
                "1fff1x78mp2c233g68my0ag0zrmjdbym8bfyahjbfy4cxza5hd33"))

(define rust-convert-case-0.7.1
  (crate-source "convert_case" "0.7.1"
                "1rzih8qbd3xh87wp76nkjvnrimn7vlzcwl2n88898ml59j6jnh5v"))

(define rust-cookie-0.16.2
  (crate-source "cookie" "0.16.2"
                "1yriqbf77iigrnp2gmf6m1r296bndv051dc1qc39w3bis1bwsng8"))

(define rust-coolor-1.1.0
  (crate-source "coolor" "1.1.0"
                "1wr7q2c8l1cmigw3h7yfdpwcz5g5xbwkirsvbjhdchxgwkyjl34q"))

(define rust-copypasta-0.10.2
  (crate-source "copypasta" "0.10.2"
                "12nkpfvr4ap56wfxyia8d4db8spfnrv3ymdwy9p29zl1gzhi2s1y"))

(define rust-core-foundation-0.10.0
  (crate-source "core-foundation" "0.10.0"
                "0qscay14s2rwkg8nd8ljhiaf149hj8sfy95d70zssy64r3jp2lmm"))

(define rust-core-foundation-0.10.1
  (crate-source "core-foundation" "0.10.1"
                "1xjns6dqf36rni2x9f47b65grxwdm20kwdg9lhmzdrrkwadcv9mj"))

(define rust-core-foundation-0.9.3
  (crate-source "core-foundation" "0.9.3"
                "0ii1ihpjb30fk38gdikm5wqlkmyr8k46fh4k2r8sagz5dng7ljhr"))

(define rust-core-foundation-0.9.4
  (crate-source "core-foundation" "0.9.4"
                "13zvbbj07yk3b61b8fhwfzhy35535a583irf23vlcg59j7h9bqci"))

(define rust-core-foundation-sys-0.8.4
  ;; TODO: Check bundled sources.
  (crate-source "core-foundation-sys" "0.8.4"
                "1yhf471qj6snnm2mcswai47vsbc9w30y4abmdp4crb4av87sb5p4"))

(define rust-core-foundation-sys-0.8.6
  ;; TODO: Check bundled sources.
  (crate-source "core-foundation-sys" "0.8.6"
                "13w6sdf06r0hn7bx2b45zxsg1mm2phz34jikm6xc5qrbr6djpsh6"))

(define rust-core-foundation-sys-0.8.7
  ;; TODO: Check bundled sources.
  (crate-source "core-foundation-sys" "0.8.7"
                "12w8j73lazxmr1z0h98hf3z623kl8ms7g07jch7n4p8f9nwlhdkp"))

(define rust-core-graphics-0.23.2
  (crate-source "core-graphics" "0.23.2"
                "10dhv3gk4kmbzl14xxkrhhky4fdp8h6nzff6h0019qgr6nz84xy0"))

(define rust-core-graphics-0.24.0
  (crate-source "core-graphics" "0.24.0"
                "1w8n8gqqm8swkanaibilqya8ryldp9fvf80byjxsaprn493a75gs"))

(define rust-core-graphics-types-0.1.3
  (crate-source "core-graphics-types" "0.1.3"
                "1bxg8nxc8fk4kxnqyanhf36wq0zrjr552c58qy6733zn2ihhwfa5"))

(define rust-core-graphics-types-0.2.0
  (crate-source "core-graphics-types" "0.2.0"
                "1sqka1rz84lr3p69i1s6lggnpnznmrw4ngc5q76w9xhky80s2i1x"))

(define rust-core-maths-0.1.1
  (crate-source "core_maths" "0.1.1"
                "0c0dv11ixxpc9bsx5xasvl98mb1dlprzcm6qq6ls3nsygw0mwx3p"))

(define rust-core-text-20.1.0
  (crate-source "core-text" "20.1.0"
                "1mffma8w0ic11ydv6zclamw4dslzmsych1fwz14msih8bh5pkln9"))

(define rust-coreaudio-rs-0.11.3
  (crate-source "coreaudio-rs" "0.11.3"
                "1kmssby4rqhv2iq1a8zmaav5p3bl40qs0wah9zv65ikr5lbpf41j"))

(define rust-coreaudio-rs-0.13.0
  (crate-source "coreaudio-rs" "0.13.0"
                "05xc1rimm02055q6y0jmsnq06rg2zdyngxwjwah7mlppp97jibhs"))

(define rust-coreaudio-sys-0.2.16
  ;; TODO: Check bundled sources.
  (crate-source "coreaudio-sys" "0.2.16"
                "0asnnypbsqzj2lxx4irnmyi9b32sl0vkxb61mj3p5mvp1fm5gs1c"))

(define rust-coreaudio-sys-0.2.17
  ;; TODO: Check bundled sources.
  (crate-source "coreaudio-sys" "0.2.17"
                "1dk4k2agjkn9ldhi9v5wjljqjjhzflx6zbrb3a9nybg6cxh7mv6f"))

(define rust-cosmic-text-0.12.1
  (crate-source "cosmic-text" "0.12.1"
                "1hl01vz4mjw3g6k5bwj7bcapilrgbv0c1kmim7zyggxl5vc5gzar"))

(define rust-cpal-0.15.3
  (crate-source "cpal" "0.15.3"
                "0yd7d51kcf8ml0bfkjrac12zgfjzk21wa97maxg0fhzpr03sngc7"))

(define rust-cpal-0.16.0
  (crate-source "cpal" "0.16.0"
                "0gxab84lvh2v7s42i7i0abh6jljbh8fpmg7qs7i9g9n27ks0glyb"))

(define rust-cpufeatures-0.2.16
  (crate-source "cpufeatures" "0.2.16"
                "1hy466fkhxjbb16i7na95wz8yr14d0kd578pwzj5lbkz14jh5f0n"))

(define rust-cpufeatures-0.2.17
  (crate-source "cpufeatures" "0.2.17"
                "10023dnnaghhdl70xcds12fsx2b966sxbxjq5sxs49mvxqw5ivar"))

(define rust-cpufeatures-0.2.6
  (crate-source "cpufeatures" "0.2.6"
                "10g16lzbczzjig8c906sjznybrb5vf0gnila7hd8ff1sicnry2i8"))

(define rust-cpufeatures-0.2.9
  (crate-source "cpufeatures" "0.2.9"
                "1wg1vmsx3gd30xkc7h7r6nfx7njx063hqjimgyrb0qj17bzpcyx1"))

(define rust-crc-3.3.0
  (crate-source "crc" "3.3.0"
                "0xg6yg57lbyzf69y8znq5gjb333w1fnlis2gnjg38blwffrx644p"))

(define rust-crc-3.4.0
  (crate-source "crc" "3.4.0"
                "03dsq5qsv86m35ikg84l80d00wnkjm8q4pjxgac0vaqjrnhs5f2y"))

(define rust-crc-catalog-2.4.0
  (crate-source "crc-catalog" "2.4.0"
                "1xg7sz82w3nxp1jfn425fvn1clvbzb3zgblmxsyqpys0dckp9lqr"))

(define rust-crc24-0.1.6
  (crate-source "crc24" "0.1.6"
                "1876c92swdpq1iv8j3y21vvfar96pxayn8rhvl42rf1yrx0if4px"))

(define rust-crc32fast-1.4.2
  (crate-source "crc32fast" "1.4.2"
                "1czp7vif73b8xslr3c9yxysmh9ws2r8824qda7j47ffs9pcnjxx9"))

(define rust-crc32fast-1.5.0
  (crate-source "crc32fast" "1.5.0"
                "04d51liy8rbssra92p0qnwjw8i9rm9c4m3bwy19wjamz1k4w30cl"))

(define rust-criterion-0.5.1
  (crate-source "criterion" "0.5.1"
                "0bv9ipygam3z8kk6k771gh9zi0j0lb9ir0xi1pc075ljg80jvcgj"))

(define rust-criterion-0.6.0
  (crate-source "criterion" "0.6.0"
                "0ya6igyv29wm1i11hscclmasc4hrr5ybslg51fyq34wqn1kazxrv"))

(define rust-criterion-0.7.0
  (crate-source "criterion" "0.7.0"
                "0a6rrgka8013l5d7bhlln60fja7nj4qmlhc4z92y3hqc5fk4gh71"))

(define rust-criterion-plot-0.5.0
  (crate-source "criterion-plot" "0.5.0"
                "1c866xkjqqhzg4cjvg01f8w6xc1j3j7s58rdksl52skq89iq4l3b"))

(define rust-criterion-plot-0.6.0
  (crate-source "criterion-plot" "0.6.0"
                "0f73hgvmxacda8xk3a2xfbrqmphcz1ail2xdhjfmkbnzqw6wq6wv"))

(define rust-critical-section-1.2.0
  (crate-source "critical-section" "1.2.0"
                "02ylhcykxjc40xrfhk1lwc21jqgz4dbwv3jr49ymw733c51yl3kr"))

(define rust-crokey-1.3.0
  (crate-source "crokey" "1.3.0"
                "1basm3p007xmbak9p9gic0lad1yk8g7sx0kcqwhdzcxyxd9hhdji"))

(define rust-crokey-proc-macros-1.3.0
  (crate-source "crokey-proc_macros" "1.3.0"
                "0fg41ah18lpgkwilh2sbjvp89kx90pr9fsl2l3hfappbr8ksgw9v"))

(define rust-crossbeam-0.8.4
  (crate-source "crossbeam" "0.8.4"
                "1a5c7yacnk723x0hfycdbl91ks2nxhwbwy46b8y5vyy0gxzcsdqi"))

(define rust-crossbeam-channel-0.5.13
  (crate-source "crossbeam-channel" "0.5.13"
                "1wkx45r34v7g3wyi3lg2wz536lrrrab4h4hh741shfhr8rlhsj1k"))

(define rust-crossbeam-channel-0.5.14
  (crate-source "crossbeam-channel" "0.5.14"
                "0wa41qybq5w8s70anb472myh4fid4aw6v65vws6wn528w9l6vfh6"))

(define rust-crossbeam-channel-0.5.15
  (crate-source "crossbeam-channel" "0.5.15"
                "1cicd9ins0fkpfgvz9vhz3m9rpkh6n8d3437c3wnfsdkd3wgif42"))

(define rust-crossbeam-channel-0.5.8
  (crate-source "crossbeam-channel" "0.5.8"
                "004jz4wxp9k26z657i7rsh9s7586dklx2c5aqf1n3w1dgzvjng53"))

(define rust-crossbeam-deque-0.8.3
  (crate-source "crossbeam-deque" "0.8.3"
                "1vqczbcild7nczh5z116w8w46z991kpjyw7qxkf24c14apwdcvyf"))

(define rust-crossbeam-deque-0.8.5
  (crate-source "crossbeam-deque" "0.8.5"
                "03bp38ljx4wj6vvy4fbhx41q8f585zyqix6pncz1mkz93z08qgv1"))

(define rust-crossbeam-deque-0.8.6
  (crate-source "crossbeam-deque" "0.8.6"
                "0l9f1saqp1gn5qy0rxvkmz4m6n7fc0b3dbm6q1r5pmgpnyvi3lcx"))

(define rust-crossbeam-epoch-0.9.14
  (crate-source "crossbeam-epoch" "0.9.14"
                "15anryfq33mhxnlw95ajixnzznxays3gpvaas6lraci7hlzmzga6"))

(define rust-crossbeam-epoch-0.9.18
  (crate-source "crossbeam-epoch" "0.9.18"
                "03j2np8llwf376m3fxqx859mgp9f83hj1w34153c7a9c7i5ar0jv"))

(define rust-crossbeam-queue-0.3.12
  (crate-source "crossbeam-queue" "0.3.12"
                "059igaxckccj6ndmg45d5yf7cm4ps46c18m21afq3pwiiz1bnn0g"))

(define rust-crossbeam-utils-0.8.15
  (crate-source "crossbeam-utils" "0.8.15"
                "0jwq8srmjcwvq9q883k9zyb26qqznaj4jjqdxmvw7xcmrkc3q1iw"))

(define rust-crossbeam-utils-0.8.20
  (crate-source "crossbeam-utils" "0.8.20"
                "100fksq5mm1n7zj242cclkw6yf7a4a8ix3lvpfkhxvdhbda9kv12"))

(define rust-crossbeam-utils-0.8.21
  (crate-source "crossbeam-utils" "0.8.21"
                "0a3aa2bmc8q35fb67432w16wvi54sfmb69rk9h5bhd18vw0c99fh"))

(define rust-crossterm-0.25.0
  (crate-source "crossterm" "0.25.0"
                "0rsbkkhdf61aipc06b7vpl4cw3wnxz0miizp0ms3a5rcpq7nqkp6"))

(define rust-crossterm-0.27.0
  (crate-source "crossterm" "0.27.0"
                "1pr413ki440xgddlmkrc4j1bfx1h8rpmll87zn8ykja1bm2gwxpl"))

(define rust-crossterm-0.28.1
  (crate-source "crossterm" "0.28.1"
                "1im9vs6fvkql0sr378dfr4wdm1rrkrvr22v4i8byz05k1dd9b7c2"))

(define rust-crossterm-0.29.0
  (crate-source "crossterm" "0.29.0"
                "0yzqxxd90k7d2ac26xq1awsznsaq0qika2nv1ik3p0vzqvjg5ffq"))

(define rust-crossterm-winapi-0.9.1
  (crate-source "crossterm_winapi" "0.9.1"
                "0axbfb2ykbwbpf1hmxwpawwfs8wvmkcka5m561l7yp36ldi7rpdc"))

(define rust-crunchy-0.2.2
  (crate-source "crunchy" "0.2.2"
                "1dx9mypwd5mpfbbajm78xcrg5lirqk7934ik980mmaffg3hdm0bs"))

(define rust-crunchy-0.2.4
  (crate-source "crunchy" "0.2.4"
                "1mbp5navim2qr3x48lyvadqblcxc1dm0lqr0swrkkwy2qblvw3s6"))

(define rust-crypto-bigint-0.5.5
  (crate-source "crypto-bigint" "0.5.5"
                "0xmbdff3g6ii5sbxjxc31xfkv9lrmyril4arh3dzckd4gjsjzj8d"))

(define rust-crypto-common-0.1.6
  (crate-source "crypto-common" "0.1.6"
                "1cvby95a6xg7kxdz5ln3rl9xh66nz66w46mm3g56ri1z5x815yqv"))

(define rust-crypto-common-0.1.7
  (crate-source "crypto-common" "0.1.7"
                "02nn2rhfy7kvdkdjl457q2z0mklcvj9h662xrq6dzhfialh2kj3q"))

(define rust-csscolorparser-0.6.2
  (crate-source "csscolorparser" "0.6.2"
                "1gxh11hajx96mf5sd0az6mfsxdryfqvcfcphny3yfbfscqq7sapb"))

(define rust-cstr-argument-0.1.2
  (crate-source "cstr-argument" "0.1.2"
                "0h7bi8sfvwq11r40pjvs3bqqmwva25dw7rasjp73niwscn79rgdn"))

(define rust-csv-1.3.1
  (crate-source "csv" "1.3.1"
                "1bzxgbbhy27flcyafxbj7f1hbn7b8wac04ijfgj34ry9m61lip5c"))

(define rust-csv-1.4.0
  (crate-source "csv" "1.4.0"
                "0f7r2ip0rbi7k377c3xmsh9xd69sillffhpfmbgnvz3yrxl9vkaj"))

(define rust-csv-core-0.1.12
  (crate-source "csv-core" "0.1.12"
                "0gfrjjlfagarhyclxrqv6b14iaxgvgc8kmwwdvw08racvaqg60kx"))

(define rust-csv-core-0.1.13
  (crate-source "csv-core" "0.1.13"
                "10lppd3fdb1i5npgx9xqjs5mjmy2qbdi8n16i48lg03ak4k3qjkh"))

(define rust-csv2svg-0.2.3
  (crate-source "csv2svg" "0.2.3"
                "0wf1ybnnrp85gnygbgi5s5inzm9yyzrc284nivxwhw4mrzgadhhw"))

(define rust-ctor-0.6.3
  (crate-source "ctor" "0.6.3"
                "03jrw316acxl3vld3wvl5m8jkj0mwwbssx7i06sb5blg4ww02kj2"))

(define rust-ctor-lite-0.1.0
  (crate-source "ctor-lite" "0.1.0"
                "0fwyn69gb0ly5lzhbxjdsp90h1s6jhaxw0rrw2n7gchs401ihy8z"))

(define rust-ctor-proc-macro-0.0.7
  (crate-source "ctor-proc-macro" "0.0.7"
                "1havwah6iryn0ang09y12xxr45jsp7ff27zflz4mhgk017ghlmjj"))

(define rust-ctrlc-3.4.4
  (crate-source "ctrlc" "3.4.4"
                "0iak582cfv3jcprd9apsy6q9glsx7n4ahiv518wcc6yw6yp6a937"))

(define rust-ctrlc-3.4.5
  (crate-source "ctrlc" "3.4.5"
                "1lqdhyl8csq8l2011g4w3wjps84w2cmwfn7jhx79ngrgm45apvlh"))

(define rust-ctrlc-3.5.1
  (crate-source "ctrlc" "3.5.1"
                "146p40m5mj6w4nncj3wpsh0dlm0r0rjyblifp8sk1xxgqj4nlwvk"))

(define rust-cursor-icon-1.1.0
  (crate-source "cursor-icon" "1.1.0"
                "14brf4vd6az9hnszwzqj7xyfaymqx9806d4i7xmwlaja3wjsr9ln"))

(define rust-cursor-icon-1.2.0
  (crate-source "cursor-icon" "1.2.0"
                "0bvkw7ak1mqwcpkgd9lh7n00hcvlh87jfl7188f231nz6zfy2ypj"))

(define rust-curve25519-dalek-4.1.3
  (crate-source "curve25519-dalek" "4.1.3"
                "1gmjb9dsknrr8lypmhkyjd67p1arb8mbfamlwxm7vph38my8pywp"))

(define rust-curve25519-dalek-derive-0.1.1
  (crate-source "curve25519-dalek-derive" "0.1.1"
                "1cry71xxrr0mcy5my3fb502cwfxy6822k4pm19cwrilrg7hq4s7l"))

(define rust-custom-debug-0.5.1
  (crate-source "custom_debug" "0.5.1"
                "1qn81isz144p531j3q4pi9lw15k0rvim0p6hjnay4ax4qbi0m7p8"))

(define rust-custom-debug-0.6.2
  (crate-source "custom_debug" "0.6.2"
                "13i9cldd9glg8k25z5ll5wb083rnl07pl7bzhwgf3cv7jnnx39rd"))

(define rust-custom-debug-derive-0.5.1
  (crate-source "custom_debug_derive" "0.5.1"
                "1xxsw5la1397b8kqfr4a1adxwjbph9s2jy52x8ngdj9l2aag7a88"))

(define rust-custom-debug-derive-0.6.2
  (crate-source "custom_debug_derive" "0.6.2"
                "04cw7wqf42rjnah5b79vpwawh94m5rjjbrrb9xicgxjjhvdcw1x7"))

(define rust-custom-derive-0.1.7
  (crate-source "custom_derive" "0.1.7"
                "1f81bavw1wnykwh21hh4yyzigs6zl6f6pkk9p3car8kq95yfb2pg"))

(define rust-custom-error-1.9.2
  (crate-source "custom_error" "1.9.2"
                "19mwa90z1hgwn3mqj07b4cy6j4yc8c59k2n99mdvm9kz37fm32jg"))

(define rust-d3d12-0.19.0
  (crate-source "d3d12" "0.19.0"
                "01x322av5z761lrgcfzyxwfpwqznc5pihlmp4k5a340221zp8g9y"))

(define rust-dark-light-1.1.1
  (crate-source "dark-light" "1.1.1"
                "0g1sq0byf5h9w12b02aza8vbrq81as4yh67yvgf419vz2sbzlxia"))

(define rust-darling-0.10.2
  (crate-source "darling" "0.10.2"
                "0n7qsp6854wm3y1q1lvylhv15zvc87ibbac1nyfmcdbyv1snww0d"))

(define rust-darling-0.14.4
  (crate-source "darling" "0.14.4"
                "0l1qrn805bsxa0iy7x8bmdwr8c10hlw0yiqs8ckv7lbz86rhqxbv"))

(define rust-darling-0.20.10
  (crate-source "darling" "0.20.10"
                "1299h2z88qn71mizhh05j26yr3ik0wnqmw11ijds89l8i9nbhqvg"))

(define rust-darling-0.20.11
  (crate-source "darling" "0.20.11"
                "1vmlphlrlw4f50z16p4bc9p5qwdni1ba95qmxfrrmzs6dh8lczzw"))

(define rust-darling-0.23.0
  (crate-source "darling" "0.23.0"
                "179fj6p6ajw4dnkrik51wjhifxwy02x5zhligyymcb905zd17bi5"))

(define rust-darling-core-0.10.2
  (crate-source "darling_core" "0.10.2"
                "16sija1jv0l754x4aa6b6fy01d1kf8m0r4id3flqipm45np61jgh"))

(define rust-darling-core-0.14.4
  (crate-source "darling_core" "0.14.4"
                "1w4b2ndxmkwghwq84yphk8x15jnpivm08w596g12ry5pwsk1r70h"))

(define rust-darling-core-0.20.10
  (crate-source "darling_core" "0.20.10"
                "1rgr9nci61ahnim93yh3xy6fkfayh7sk4447hahawah3m1hkh4wm"))

(define rust-darling-core-0.20.11
  (crate-source "darling_core" "0.20.11"
                "0bj1af6xl4ablnqbgn827m43b8fiicgv180749f5cphqdmcvj00d"))

(define rust-darling-core-0.23.0
  (crate-source "darling_core" "0.23.0"
                "1c033vrks38vpw8kwgd5w088dsr511kfz55n9db56prkgh7sarcq"))

(define rust-darling-macro-0.10.2
  (crate-source "darling_macro" "0.10.2"
                "0wlv31cxkrjijz5gv13hvk55c9lmd781aj12c8n84sa9mksa5dfr"))

(define rust-darling-macro-0.14.4
  (crate-source "darling_macro" "0.14.4"
                "13mlyd5w275c815k0ijf6g4c446hs8b3m2h4an5isqgpr7dv9am4"))

(define rust-darling-macro-0.20.10
  (crate-source "darling_macro" "0.20.10"
                "01kq3ibbn47czijj39h3vxyw0c2ksd0jvc097smcrk7n2jjs4dnk"))

(define rust-darling-macro-0.20.11
  (crate-source "darling_macro" "0.20.11"
                "1bbfbc2px6sj1pqqq97bgqn6c8xdnb2fmz66f7f40nrqrcybjd7w"))

(define rust-darling-macro-0.23.0
  (crate-source "darling_macro" "0.23.0"
                "13fvzji9xyp304mgq720z5l0xgm54qj68jibwscagkynggn88fdc"))

(define rust-dashmap-5.5.3
  (crate-source "dashmap" "5.5.3"
                "0miqnlxi501vfbv6mw5jbmzgnj0wjrch3p4abvpd59s9v30lg1wp"))

(define rust-dashmap-6.1.0
  (crate-source "dashmap" "6.1.0"
                "1kvnw859xvrqyd1lk89na6797yvl5bri4wi9j0viz2a4j54wqhah"))

(define rust-dasp-sample-0.11.0
  (crate-source "dasp_sample" "0.11.0"
                "0zzw35akm3qs2rixbmlijk6h0l4g9ry6g74qc59zv1q8vs1f31qc"))

(define rust-data-encoding-2.10.0
  (crate-source "data-encoding" "2.10.0"
                "1shzipi8igi058fkx9wfiy6prd7d8rahz1lb7d4idw9nfvrf58fp"))

(define rust-data-encoding-2.6.0
  (crate-source "data-encoding" "2.6.0"
                "1qnn68n4vragxaxlkqcb1r28d3hhj43wch67lm4rpxlw89wnjmp8"))

(define rust-data-encoding-2.9.0
  (crate-source "data-encoding" "2.9.0"
                "0xm46371aw613ghc12ay4vsnn49hpcmcwlijnqy8lbp2bpd308ra"))

(define rust-data-url-0.3.1
  (crate-source "data-url" "0.3.1"
                "0ahclz72myi350cs1xcsxdh1v0iljpfj4ghcy2fy46mpfhf7laaw"))

(define rust-data-url-0.3.2
  (crate-source "data-url" "0.3.2"
                "0xl30jidc8s3kh2z3nvnn1nyzhbq5b2wpiqwzj9gjdrndk50n7my"))

(define rust-datatest-stable-0.3.3
  (crate-source "datatest-stable" "0.3.3"
                "10nxz8m2xaffwqz04hgfqnwv6p5l4mx3h9jliakg775n5qrdfrx8"))

(define rust-dbus-0.9.7
  (crate-source "dbus" "0.9.7"
                "06vdv4aarjs4w6byg9nqajr67c8qvlhk3153ic2i65pvp63ikchv"))

(define rust-dbus-crossroads-0.5.2
  (crate-source "dbus-crossroads" "0.5.2"
                "1q3dyywazr3hppm052fa8q2366q66ml789r42jjlnm47f51q6k1s"))

(define rust-dbus-secret-service-4.0.3
  (crate-source "dbus-secret-service" "4.0.3"
                "0jyqdya8zgddgwsrzf2gs9qqwz9043qv2idffgnjmnc18hvicaml"))

(define rust-dbus-tokio-0.7.6
  (crate-source "dbus-tokio" "0.7.6"
                "04xd3z2dnjv4d45kj3wqnwbnwllrp1zsg8v3q0qp2rxwb7a8hxh0"))

(define rust-dconf-rs-0.3.0
  (crate-source "dconf_rs" "0.3.0"
                "12swi0npq88kbdwnc3n324dzknir674agrh13h305876h654cikh"))

(define rust-ddc-0.2.2
  (crate-source "ddc" "0.2.2"
                "1if6nf8mkv49fls7z47931ps7l84pyxh5jqpmnmw83rj7v2z4sds"))

(define rust-ddc-hi-0.4.1
  (crate-source "ddc-hi" "0.4.1"
                "1pllvvsmra839jdgdqphpaky69n7dhd0zcrr8yijlsljgjqlfrww"))

(define rust-ddc-i2c-0.2.2
  (crate-source "ddc-i2c" "0.2.2"
                "06v9w86bja1r4hklcayvkss62wqzn23hia45gc61vhfmkyn8zw8y"))

(define rust-ddc-macos-0.2.2
  (crate-source "ddc-macos" "0.2.2"
                "1p1lllbxx5c1h79jamgd899dz2jq02ds2sfjlm0sygdk60pa5b18"))

(define rust-ddc-winapi-0.2.2
  (crate-source "ddc-winapi" "0.2.2"
                "0dff9cmdx0f73cyn877bwwqg1r462sav8bf8w1vlrxdxaqsnjrav"))

(define rust-deflate-1.0.0
  (crate-source "deflate" "1.0.0"
                "0bs319wa9wl7pn9j6jrrxg1gaqbak581rkx210cbix0qyljpwvy8"))

(define rust-deltae-0.3.2
  (crate-source "deltae" "0.3.2"
                "1d3hw9hpvicl9x0x34jr2ybjk5g5ym1lhbyz6zj31110gq8zaaap"))

(define rust-der-0.7.9
  (crate-source "der" "0.7.9"
                "1h4vzjfa1lczxdf8avfj9qlwh1qianqlxdy1g5rn762qnvkzhnzm"))

(define rust-der-parser-10.0.0
  (crate-source "der-parser" "10.0.0"
                "19n13gjidjcbj23ps6fww322zx8mz4kfs4cvsd6kqnjx84b51nh7"))

(define rust-deranged-0.3.11
  (crate-source "deranged" "0.3.11"
                "1d1ibqqnr5qdrpw8rclwrf1myn3wf0dygl04idf4j2s49ah6yaxl"))

(define rust-deranged-0.3.8
  (crate-source "deranged" "0.3.8"
                "0ikrhil2621rz9haakphdzrx035qwr175f639p8qyrazjj56wsgj"))

(define rust-deranged-0.5.4
  (crate-source "deranged" "0.5.4"
                "0wch36gpg2crz2f72p7c0i5l4bzxjkwxw96sdj57c1cadzw566d4"))

(define rust-deranged-0.5.5
  (crate-source "deranged" "0.5.5"
                "11z5939gv2klp1r1lgrp4w5fnlkj18jqqf0h9zxmia3vkrjwpv7c"))

(define rust-derivative-2.2.0
  (crate-source "derivative" "2.2.0"
                "02vpb81wisk2zh1d5f44szzxamzinqgq2k8ydrfjj2wwkrgdvhzw"))

(define rust-derive-builder-0.12.0
  (crate-source "derive_builder" "0.12.0"
                "1y4p569zcvpmly5s5hmjp9h83drxvdp6kj6bb61h225mhj3pfrwd"))

(define rust-derive-builder-0.20.2
  (crate-source "derive_builder" "0.20.2"
                "0is9z7v3kznziqsxa5jqji3ja6ay9wzravppzhcaczwbx84znzah"))

(define rust-derive-builder-core-0.12.0
  (crate-source "derive_builder_core" "0.12.0"
                "03vvmw3mfg370swq0dh2h5kcjjb8va2m4asqgp9wfyy4l08xq6y1"))

(define rust-derive-builder-core-0.20.2
  (crate-source "derive_builder_core" "0.20.2"
                "1s640r6q46c2iiz25sgvxw3lk6b6v5y8hwylng7kas2d09xwynrd"))

(define rust-derive-builder-macro-0.12.0
  (crate-source "derive_builder_macro" "0.12.0"
                "17p71qzh7x1q2yxzz3xrg73zw3xl0h479b7ybyjm0s1rg9fa7kgb"))

(define rust-derive-builder-macro-0.20.2
  (crate-source "derive_builder_macro" "0.20.2"
                "0g1zznpqrmvjlp2w7p0jzsjvpmw5rvdag0rfyypjhnadpzib0qxb"))

(define rust-derive-getters-0.5.0
  (crate-source "derive-getters" "0.5.0"
                "1zyah2rr8jdy8a0cnhnr48dxg8f6alkr59fkfynh273h7ra47vvl"))

(define rust-derive-is-enum-variant-0.1.1
  (crate-source "derive_is_enum_variant" "0.1.1"
                "15w18649m4h47pdpr04id0wv8br8bg606zvrafcrfijihicqib6h"))

(define rust-derive-more-0.99.17
  (crate-source "derive_more" "0.99.17"
                "0883jr0gdf9mi8rn7qbz1p0z7hqzfd275pil2gwm66bw1b9i1f2g"))

(define rust-derive-more-2.0.1
  (crate-source "derive_more" "2.0.1"
                "0y3n97cc7rsvgnj211p92y1ppzh6jzvq5kvk6340ghkhfp7l4ch9"))

(define rust-derive-more-2.1.1
  (crate-source "derive_more" "2.1.1"
                "0d5i10l4aff744jw7v4n8g6cv15rjk5mp0f1z522pc2nj7jfjlfp"))

(define rust-derive-more-impl-2.0.1
  (crate-source "derive_more-impl" "2.0.1"
                "1wqxcb7d5lzvpplz9szp4rwy1r23f5wmixz0zd2vcjscqknji9mx"))

(define rust-derive-more-impl-2.1.1
  (crate-source "derive_more-impl" "2.1.1"
                "1jwdp836vymp35d7mfvvalplkdgk2683nv3zjlx65n1194k9g6kr"))

(define rust-derive-setters-0.1.8
  (crate-source "derive_setters" "0.1.8"
                "1jf08hr4k8zd1vk9196pdcbl5rb03j6ribzc0s624k0hv9g64p5f"))

(define rust-des-0.8.1
  (crate-source "des" "0.8.1"
                "07kshslxanmg0g6007scvglfhg6mli2a8qzhx4kxx4z9ik781pgz"))

(define rust-deser-hjson-2.2.4
  (crate-source "deser-hjson" "2.2.4"
                "1qc82vh683wb3359fs5r9fwi37wjnb17zfmrwkbxw22w172am53x"))

(define rust-detect-desktop-environment-0.2.0
  (crate-source "detect-desktop-environment" "0.2.0"
                "0408nmfgb54wqigjpqxwcxy5jp4ni1fjvynqdgpa84svvmhavn11"))

(define rust-devil-0.0.7
  (crate-source "devil" "0.0.7"
                "1m7lgddqyrnkfyhniaja2kdkm4y8pq6p01scf2xazbcygicv0606"))

(define rust-diff-0.1.13
  (crate-source "diff" "0.1.13"
                "1j0nzjxci2zqx63hdcihkp0a4dkdmzxd7my4m7zk6cjyfy34j9an"))

(define rust-difflib-0.4.0
  (crate-source "difflib" "0.4.0"
                "1s7byq4d7jgf2hcp2lcqxi2piqwl8xqlharfbi8kf90n8csy7131"))

(define rust-digest-0.10.6
  (crate-source "digest" "0.10.6"
                "0vz74785s96g727vg37iwkjvbkcfzp093j49ihhyf8sh9s7kfs41"))

(define rust-digest-0.10.7
  (crate-source "digest" "0.10.7"
                "14p2n6ih29x81akj097lvz7wi9b6b9hvls0lwrv7b6xwyy0s5ncy"))

(define rust-directories-4.0.1
  (crate-source "directories" "4.0.1"
                "045jbj5y2f1fmjs9rfcw95y0vjydb2rqqhz1sdnqhdmxv96ms77m"))

(define rust-directories-5.0.1
  (crate-source "directories" "5.0.1"
                "0dba6xzk79s1clqzxh2qlgzk3lmvvks1lzzjhhi3hd70hhxifjcs"))

(define rust-dirs-4.0.0
  (crate-source "dirs" "4.0.0"
                "0n8020zl4f0frfnzvgb9agvk4a14i1kjz4daqnxkgslndwmaffna"))

(define rust-dirs-5.0.1
  (crate-source "dirs" "5.0.1"
                "0992xk5vx75b2x91nw9ssb51mpl8x73j9rxmpi96cryn0ffmmi24"))

(define rust-dirs-6.0.0
  (crate-source "dirs" "6.0.0"
                "0knfikii29761g22pwfrb8d0nqpbgw77sni9h2224haisyaams63"))

(define rust-dirs-next-1.0.2
  (crate-source "dirs-next" "1.0.2"
                "1dl2dqzsrcb7qigfiwpdpakhdkpz0629pvylbj2ylyrkh1dfcdng"))

(define rust-dirs-sys-0.3.7
  ;; TODO: Check bundled sources.
  (crate-source "dirs-sys" "0.3.7"
                "19md1cnkazham8a6kh22v12d8hh3raqahfk6yb043vrjr68is78v"))

(define rust-dirs-sys-0.4.1
  ;; TODO: Check bundled sources.
  (crate-source "dirs-sys" "0.4.1"
                "071jy0pvaad9lsa6mzawxrh7cmr7hsmsdxwzm7jzldfkrfjha3sj"))

(define rust-dirs-sys-0.5.0
  ;; TODO: Check bundled sources.
  (crate-source "dirs-sys" "0.5.0"
                "1aqzpgq6ampza6v012gm2dppx9k35cdycbj54808ksbys9k366p0"))

(define rust-dirs-sys-next-0.1.2
  (crate-source "dirs-sys-next" "0.1.2"
                "0kavhavdxv4phzj4l0psvh55hszwnr0rcz8sxbvx20pyqi2a3gaf"))

(define rust-discard-1.0.4
  (crate-source "discard" "1.0.4"
                "1h67ni5bxvg95s91wgicily4ix7lcw7cq0a5gy9njrybaibhyb91"))

(define rust-dispatch-0.2.0
  (crate-source "dispatch" "0.2.0"
                "0fwjr9b7582ic5689zxj8lf7zl94iklhlns3yivrnv8c9fxr635x"))

(define rust-dispatch2-0.3.0
  (crate-source "dispatch2" "0.3.0"
                "1v1ak9w0s8z1g13x4mj2y5im9wmck0i2vf8f8wc9l1n6lqi9z849"))

(define rust-displaydoc-0.2.4
  (crate-source "displaydoc" "0.2.4"
                "0p8pyg10csc782qlwx3znr6qx46ni96m1qh597kmyrf6s3s8axa8"))

(define rust-displaydoc-0.2.5
  (crate-source "displaydoc" "0.2.5"
                "1q0alair462j21iiqwrr21iabkfnb13d6x5w95lkdg21q2xrqdlp"))

(define rust-dlib-0.5.2
  (crate-source "dlib" "0.5.2"
                "04m4zzybx804394dnqs1blz241xcy480bdwf3w9p4k6c3l46031k"))

(define rust-dlv-list-0.3.0
  (crate-source "dlv-list" "0.3.0"
                "0mqj5rdkcjksw3kvjj0nga6rzcpppx0kimjwi527yhifz6kw5206"))

(define rust-dns-lookup-2.0.4
  (crate-source "dns-lookup" "2.0.4"
                "1z74n2zij2gahycabm0gkmkyx574h76gwk7sz93yqpr3qa3n0xp5"))

(define rust-dns-lookup-3.0.1
  (crate-source "dns-lookup" "3.0.1"
                "07f4lvzsf0p80qh2haiq01fvpi0rk1lf783bnsxzb8i1xr606fbf"))

(define rust-doc-comment-0.3.3
  (crate-source "doc-comment" "0.3.3"
                "043sprsf3wl926zmck1bm7gw0jq50mb76lkpk49vasfr6ax1p97y"))

(define rust-doc-comment-0.3.4
  (crate-source "doc-comment" "0.3.4"
                "1j8jbrw8335hciwn3h2idkfc3kmx3pfn0sxcwjw1m8lmn6w5a2bq"))

(define rust-document-features-0.2.11
  (crate-source "document-features" "0.2.11"
                "0pdhpbz687fk2rkgz45yy3gvbhlxliwb7g1lj3jbx1f1qr89n94m"))

(define rust-document-features-0.2.12
  (crate-source "document-features" "0.2.12"
                "0qcgpialq3zgvjmsvar9n6v10rfbv6mk6ajl46dd4pj5hn3aif6l"))

(define rust-domain-0.10.3
  (crate-source "domain" "0.10.3"
                "13f2l5g9890v1ilgn6z16y09p1as7a7ssa6dcf5aidpkv5k8c034"))

(define rust-domain-0.11.0
  (crate-source "domain" "0.11.0"
                "1ygyzv3r59d9lzpdlb7mzlp8pbx7bqgy7ijk07m2lvba9bqdf7d1"))

(define rust-domain-macros-0.11.0
  (crate-source "domain-macros" "0.11.0"
                "0w3b6zviddzz2j5l83c9wb6lxl6msnpg7p4dgzmzvdfdsbgpy68f"))

(define rust-downcast-0.11.0
  (crate-source "downcast" "0.11.0"
                "1wa78ahlc57wmqyq2ncr80l7plrkgz57xsg7kfzgpcnqac8gld8l"))

(define rust-downcast-rs-1.2.0
  (crate-source "downcast-rs" "1.2.0"
                "0l36kgxqd5djhqwf5abxjmgasdw8n0qsjvw3jdvhi91nj393ba4y"))

(define rust-downcast-rs-1.2.1
  (crate-source "downcast-rs" "1.2.1"
                "1lmrq383d1yszp7mg5i7i56b17x2lnn3kb91jwsq0zykvg2jbcvm"))

(define rust-dpi-0.1.1
  (crate-source "dpi" "0.1.1"
                "0lzz48gpgbwdrw0s8vib0589ij9jizv1vzsphm4xd9kw58lhwp7j"))

(define rust-dpi-0.1.2
  (crate-source "dpi" "0.1.2"
                "0xhsvzgjvdch2fwmfc9vkb708b0q59b6imypyjlgbiigyb74rcfq"))

(define rust-drm-0.12.0
  (crate-source "drm" "0.12.0"
                "1ldmrglplhrcfmpyjy2ga4hg89c89f0kzmky3b0j85b0pm5qr24q"))

(define rust-drm-ffi-0.8.0
  (crate-source "drm-ffi" "0.8.0"
                "0lwd4zvimd4132cpay3vs41gf1sv3s4gx37a997wnz4bwhkqgjcp"))

(define rust-drm-fourcc-2.2.0
  (crate-source "drm-fourcc" "2.2.0"
                "1x76v9a0pkgym4n6cah4barnai9gsssm7gjzxskw2agwibdvrbqa"))

(define rust-drm-sys-0.7.0
  ;; TODO: Check bundled sources.
  (crate-source "drm-sys" "0.7.0"
                "11irvbzb0rbsm5zxgxsbaca8f6nvkl8kvwk3hwp6q6bf1gjdsfgx"))

(define rust-dtoa-0.4.8
  (crate-source "dtoa" "0.4.8"
                "1c5j0wz118dhrczx6spc5za7dnbfxablr4adyahg9aknrsc9i2an"))

(define rust-dtor-0.1.1
  (crate-source "dtor" "0.1.1"
                "00fkcw8zn0g10m4k8b0qgmn304g47xqwn1ihhzyjra48n3p04ka0"))

(define rust-dtor-proc-macro-0.0.6
  (crate-source "dtor-proc-macro" "0.0.6"
                "19fg0mivy9qyvbwmqj3ysj0qm5cay0gyp5fyw1imq89cj95cyy7n"))

(define rust-dunce-1.0.5
  (crate-source "dunce" "1.0.5"
                "04y8wwv3vvcqaqmqzssi6k0ii9gs6fpz96j5w9nky2ccsl23axwj"))

(define rust-dwrote-0.11.5
  (crate-source "dwrote" "0.11.5"
                "00jd7grrf7sz10y4zzipl0z8s27sbpirdl7fkjqwib1j4i9ka6wy"))

(define rust-dyn-clone-1.0.17
  (crate-source "dyn-clone" "1.0.17"
                "09cig7dgg6jnqa10p4233nd8wllbjf4ffsw7wj0m4lwa5w3z0vhd"))

(define rust-dyn-clone-1.0.18
  (crate-source "dyn-clone" "1.0.18"
                "0dag651ph5q0mcax74y4m1k9hyl737q1v03isck3mzxsfd7g9vpy"))

(define rust-dyn-clone-1.0.20
  (crate-source "dyn-clone" "1.0.20"
                "0m956cxcg8v2n8kmz6xs5zl13k2fak3zkapzfzzp7pxih6hix26h"))

(define rust-easy-cast-0.5.4
  (crate-source "easy-cast" "0.5.4"
                "0ay3h4irzyiml1w1r4fa64vbhb6cpwjdzv5xfi32xz19q8whbx13"))

(define rust-ecdsa-0.16.9
  (crate-source "ecdsa" "0.16.9"
                "1jhb0bcbkaz4001sdmfyv8ajrv8a1cg7z7aa5myrd4jjbhmz69zf"))

(define rust-ed25519-2.2.3
  (crate-source "ed25519" "2.2.3"
                "0lydzdf26zbn82g7xfczcac9d7mzm3qgx934ijjrd5hjpjx32m8i"))

(define rust-ed25519-dalek-2.1.1
  (crate-source "ed25519-dalek" "2.1.1"
                "0w88cafwglg9hjizldbmlza0ns3hls81zk1bcih3m5m3h67algaa"))

(define rust-edid-0.3.0
  (crate-source "edid" "0.3.0"
                "1zi4md5sy60nlcy45d3w9r48ki6ywzh7rdivzvf39n4k119pbki4"))

(define rust-either-1.13.0
  (crate-source "either" "1.13.0"
                "1w2c1mybrd7vljyxk77y9f4w9dyjrmp3yp82mk7bcm8848fazcb0"))

(define rust-either-1.14.0
  (crate-source "either" "1.14.0"
                "17fs0r9mnj632k4ff8c6zyq80zqvqb0wa9cgsyd5iprd159l74dp"))

(define rust-either-1.15.0
  (crate-source "either" "1.15.0"
                "069p1fknsmzn9llaizh77kip0pqmcwpdsykv2x30xpjyija5gis8"))

(define rust-either-1.8.1
  (crate-source "either" "1.8.1"
                "14bdy4qsxlfnm4626z4shwaiffi8l5krzkn7ykki1jgqzsrapjkz"))

(define rust-elf-0.8.0
  (crate-source "elf" "0.8.0"
                "0w00dga7qqrvyd8s51snw3dji2iyxq2s6amzgaapxi9z4658ipam"))

(define rust-elliptic-curve-0.13.8
  (crate-source "elliptic-curve" "0.13.8"
                "0ixx4brgnzi61z29r3g1606nh2za88hzyz8c5r3p6ydzhqq09rmm"))

(define rust-email-address-0.2.9
  (crate-source "email_address" "0.2.9"
                "0jf4v3npa524c7npy7w3jl0a6gng26f51a4bgzs3jqna12dz2yg0"))

(define rust-email-lib-0.26.4
  (crate-source "email-lib" "0.26.4"
                "00pydx36j9k7f1lrbc5scxqr15j6a9msc7ad76yf4a3hggaqs1j0"))

(define rust-email-macros-0.0.2
  (crate-source "email-macros" "0.0.2"
                "15bw0jpdcgfybq9b6ak2mfdwn5c76l92r8gqcj3py0jissgs090g"))

(define rust-encode-unicode-0.3.6
  (crate-source "encode_unicode" "0.3.6"
                "07w3vzrhxh9lpjgsg2y5bwzfar2aq35mdznvcp3zjl0ssj7d4mx3"))

(define rust-encode-unicode-1.0.0
  (crate-source "encode_unicode" "1.0.0"
                "1h5j7j7byi289by63s3w4a8b3g6l5ccdrws7a67nn07vdxj77ail"))

(define rust-encoding-rs-0.8.34
  (crate-source "encoding_rs" "0.8.34"
                "0nagpi1rjqdpvakymwmnlxzq908ncg868lml5b70n08bm82fjpdl"))

(define rust-encoding-rs-0.8.35
  (crate-source "encoding_rs" "0.8.35"
                "1wv64xdrr9v37rqqdjsyb8l8wzlcbab80ryxhrszvnj59wy0y0vm"))

(define rust-endi-1.1.0
  (crate-source "endi" "1.1.0"
                "1gxp388g2zzbncp3rdn60wxkr49xbhhx94nl9p4a6c41w4ma7n53"))

(define rust-enum-as-inner-0.6.1
  (crate-source "enum-as-inner" "0.6.1"
                "1g3cywc65d9w974l2xy86ij13njss3qjc7b0kfbzbws9qrjs5rm1"))

(define rust-enum-dispatch-0.3.13
  (crate-source "enum_dispatch" "0.3.13"
                "1kby2jz173ggg7wk41vjsskmkdyx7749ll8lhqhv6mb5qqmww65a"))

(define rust-enum-kinds-0.5.1
  (crate-source "enum-kinds" "0.5.1"
                "0qnlgzlsydnxsgcf2lkvqsrmdxignjkam1fsnfd4c7b8amls2h2f"))

(define rust-enumflags2-0.7.10
  (crate-source "enumflags2" "0.7.10"
                "0g8kmhaqxq44v76wh971biljrgaqbjc8fbyw2d1z3wsnb5zxncnj"))

(define rust-enumflags2-0.7.11
  (crate-source "enumflags2" "0.7.11"
                "0iwi60d54lgby0f29b5isikxraf0wvnqdmlddx68a62kbx34nbxs"))

(define rust-enumflags2-0.7.12
  (crate-source "enumflags2" "0.7.12"
                "1vzcskg4dca2jiflsfx1p9yw1fvgzcakcs7cpip0agl51ilgf9qh"))

(define rust-enumflags2-0.7.7
  (crate-source "enumflags2" "0.7.7"
                "1lhvq084ylw3nvhgv1zyagavkj392zamydh5v6y352zn1l4zahf0"))

(define rust-enumflags2-derive-0.7.10
  (crate-source "enumflags2_derive" "0.7.10"
                "1s29iqx3gj5l5s19a22lpn0nljhp5l9smqac99126n2qhfhlh3fy"))

(define rust-enumflags2-derive-0.7.11
  (crate-source "enumflags2_derive" "0.7.11"
                "0yfdjyrf9b4mi1r589azkyirjhzmdw29nqq0mdjnsyldlmjayk7w"))

(define rust-enumflags2-derive-0.7.12
  (crate-source "enumflags2_derive" "0.7.12"
                "09rqffacafl1b83ir55hrah9gza0x7pzjn6lr6jm76fzix6qmiv7"))

(define rust-enumflags2-derive-0.7.7
  (crate-source "enumflags2_derive" "0.7.7"
                "0iapq76azgkqxby4v117a3jssa9rz7n18vla4i09grc3gngiz6jy"))

(define rust-env-filter-0.1.3
  (crate-source "env_filter" "0.1.3"
                "1l4p6f845cylripc3zkxa0lklk8rn2q86fqm522p6l2cknjhavhq"))

(define rust-env-home-0.1.0
  (crate-source "env_home" "0.1.0"
                "1zn08mk95rjh97831rky1n944k024qrwjhbcgb0xv9zhrh94xy67"))

(define rust-env-logger-0.11.6
  (crate-source "env_logger" "0.11.6"
                "1q30cqb2dfs3qrs0s30qdmqwi7n2gz4pniwd8a9gvhygwgcf7bnw"))

(define rust-env-logger-0.4.3
  (crate-source "env_logger" "0.4.3"
                "0nydz2lidsvx9gs0v2zcz68rzqx8in7fzmiprgsrhqh17vkj3prx"))

(define rust-equator-0.4.2
  (crate-source "equator" "0.4.2"
                "1z760z5r0haxjyakbqxvswrz9mq7c29arrivgq8y1zldhc9v44a7"))

(define rust-equator-macro-0.4.2
  (crate-source "equator-macro" "0.4.2"
                "1cqzx3cqn9rxln3a607xr54wippzff56zs5chqdf3z2bnks3rwj4"))

(define rust-equivalent-1.0.1
  (crate-source "equivalent" "1.0.1"
                "1malmx5f4lkfvqasz319lq6gb3ddg19yzf9s8cykfsgzdmyq0hsl"))

(define rust-equivalent-1.0.2
  (crate-source "equivalent" "1.0.2"
                "03swzqznragy8n0x31lqc78g2af054jwivp7lkrbrc0khz74lyl7"))

(define rust-erased-serde-0.4.5
  (crate-source "erased-serde" "0.4.5"
                "13dirfj9972nvk05b20w3xyn3xp1j6qyfp9avhksnkxbcnfkiqi4"))

(define rust-erased-serde-0.4.8
  (crate-source "erased-serde" "0.4.8"
                "0jxx1d3ba93c8l3710acl3z45d5fb12xjc9f6ccyr3c1156l1795"))

(define rust-errno-0.2.8
  (crate-source "errno" "0.2.8"
                "18cnqgk8r6lq1n5cfy3bryiyz9zkqr10dxj49sa3fkzfamih8fgn"))

(define rust-errno-0.3.10
  (crate-source "errno" "0.3.10"
                "0pgblicz1kjz9wa9m0sghkhh2zw1fhq1mxzj7ndjm746kg5m5n1k"))

(define rust-errno-0.3.11
  (crate-source "errno" "0.3.11"
                "0kjrrcaa5nvickysw7z3vm5p0l7l457idf1ff3z6ang8qwnx8vcp"))

(define rust-errno-0.3.13
  (crate-source "errno" "0.3.13"
                "1bd5g3srn66zr3bspac0150bvpg1s7zi6zwhwhlayivciz12m3kp"))

(define rust-errno-0.3.14
  (crate-source "errno" "0.3.14"
                "1szgccmh8vgryqyadg8xd58mnwwicf39zmin3bsn63df2wbbgjir"))

(define rust-errno-0.3.3
  (crate-source "errno" "0.3.3"
                "1pfv4gygg742cwi21gw88h4f7q5kvwkpk7b3xxpmrqh8hlc2cr8k"))

(define rust-errno-0.3.8
  (crate-source "errno" "0.3.8"
                "0ia28ylfsp36i27g1qih875cyyy4by2grf80ki8vhgh6vinf8n52"))

(define rust-errno-0.3.9
  (crate-source "errno" "0.3.9"
                "1fi0m0493maq1jygcf1bya9cymz2pc1mqxj26bdv7yjd37v5qk2k"))

(define rust-errno-dragonfly-0.1.2
  (crate-source "errno-dragonfly" "0.1.2"
                "1grrmcm6q8512hkq5yzch3yv8wafflc2apbmsaabiyk44yqz2s5a"))

(define rust-error-code-2.3.1
  (crate-source "error-code" "2.3.1"
                "08baxlf8qz01lgjsdbfhs193r9y1nlc566s5xvzyf4dzwy8qkwb4"))

(define rust-error-code-3.3.1
  (crate-source "error-code" "3.3.1"
                "0bx9hw3pahzqym8jvb0ln4qsabnysgn98mikyh2afhk9rif31nd5"))

(define rust-error-code-3.3.2
  (crate-source "error-code" "3.3.2"
                "0nacxm9xr3s1rwd6fabk3qm89fyglahmbi4m512y0hr8ym6dz8ny"))

(define rust-escape8259-0.5.3
  (crate-source "escape8259" "0.5.3"
                "1in2zrdak794x8v11lkrrgyqxjjqbijvg0yfn2paay0rb9xxv4jn"))

(define rust-etagere-0.2.13
  (crate-source "etagere" "0.2.13"
                "15g49vsv5syjfvmkcpj4dh3b8xc67q0vzhg8kda0zcczw4xiwbqf"))

(define rust-etagere-0.2.15
  (crate-source "etagere" "0.2.15"
                "0hjkik00nl8bm9fafpzqkb6yah3mkphc21zpc159a5fwwncvz2gw"))

(define rust-etcetera-0.10.0
  (crate-source "etcetera" "0.10.0"
                "1rka6bskn93pdhx32xaagr147q95z5bnz7ym5xr85jw00wyv3ir6"))

(define rust-etcetera-0.11.0
  (crate-source "etcetera" "0.11.0"
                "15myc4rl62iah8acdl1sxmrdxb8ci55zbphrv07s55qx3i6wqj6y"))

(define rust-etherparse-0.16.0
  (crate-source "etherparse" "0.16.0"
                "0wvd1a3d7y3q6giy4s20ih07ymzp8p6j613shsflwj0pnq2agn5q"))

(define rust-etherparse-0.19.0
  (crate-source "etherparse" "0.19.0"
                "1iyif944ck766xl4y9cgqhqds9izc6rvi50348d7a07qdxwvj6di"))

(define rust-euclid-0.22.11
  (crate-source "euclid" "0.22.11"
                "0j4yb01x9dn5hbbbigd3mwdplv4m29k5drmhmc95lj3yfi5xp75d"))

(define rust-evdev-rs-0.6.3
  (crate-source "evdev-rs" "0.6.3"
                "16l8d3sa3h7rz3b5ay0jq2b74vpsby5hx3qvx4m570yqirivb2nj"))

(define rust-evdev-sys-0.2.6
  ;; TODO: Check bundled sources.
  (crate-source "evdev-sys" "0.2.6"
                "0xp55m46xlswzrx98zxn7g9nikwgvlpvjd9v5fn816jdkx40vkyd"))

(define rust-event-listener-2.5.3
  (crate-source "event-listener" "2.5.3"
                "1q4w3pndc518crld6zsqvvpy9lkzwahp2zgza9kbzmmqh9gif1h2"))

(define rust-event-listener-5.3.1
  (crate-source "event-listener" "5.3.1"
                "1fkm6q4hjn61wl52xyqyyxai0x9w0ngrzi0wf1qsf8vhsadvwck0"))

(define rust-event-listener-5.4.0
  (crate-source "event-listener" "5.4.0"
                "1bii2gn3vaa33s0gr2zph7cagiq0ppcfxcxabs24ri9z9kgar4il"))

(define rust-event-listener-5.4.1
  (crate-source "event-listener" "5.4.1"
                "1asnp3agbr8shcl001yd935m167ammyi8hnvl0q1ycajryn6cfz1"))

(define rust-event-listener-strategy-0.5.2
  (crate-source "event-listener-strategy" "0.5.2"
                "18f5ri227khkayhv3ndv7yl4rnasgwksl2jhwgafcxzr7324s88g"))

(define rust-event-listener-strategy-0.5.3
  (crate-source "event-listener-strategy" "0.5.3"
                "1ch5gf6knllyq12jkb5zdfag573dh44307q4pwwi2g37sc6lwgiw"))

(define rust-event-listener-strategy-0.5.4
  (crate-source "event-listener-strategy" "0.5.4"
                "14rv18av8s7n8yixg38bxp5vg2qs394rl1w052by5npzmbgz7scb"))

(define rust-exr-1.73.0
  (crate-source "exr" "1.73.0"
                "1q47yq78q9k210r6jy1wwrilxwwxqavik9l3l426rd17k7srfcgq"))

(define rust-eyre-0.6.12
  (crate-source "eyre" "0.6.12"
                "1v1a3vb9gs5zkwp4jzkcfnpg0gvyp4ifydzx37f4qy14kzcibnbw"))

(define rust-eyre-0.6.8
  (crate-source "eyre" "0.6.8"
                "1sy7x8p74jfx1mvj4ifl0lxkxaqvmswdgdr84y1dqb6055d6nasc"))

(define rust-fallible-iterator-0.3.0
  (crate-source "fallible-iterator" "0.3.0"
                "0ja6l56yka5vn4y4pk6hn88z0bpny7a8k1919aqjzp0j1yhy9k1a"))

(define rust-fallible-streaming-iterator-0.1.9
  (crate-source "fallible-streaming-iterator" "0.1.9"
                "0nj6j26p71bjy8h42x6jahx1hn0ng6mc2miwpgwnp8vnwqf4jq3k"))

(define rust-fancy-regex-0.11.0
  (crate-source "fancy-regex" "0.11.0"
                "18j0mmzfycibhxhhhfja00dxd1vf8x5c28lbry224574h037qpxr"))

(define rust-fancy-regex-0.14.0
  (crate-source "fancy-regex" "0.14.0"
                "162j2qx2ikgl79grq12mawyflwkirnjzrvxh11a1xbmwjidcn93f"))

(define rust-fancy-regex-0.16.1
  (crate-source "fancy-regex" "0.16.1"
                "0z4lv4madmsl2536smbm6jcqiqpc7ail1d57an1wwjj62pnca15z"))

(define rust-fancy-regex-0.16.2
  (crate-source "fancy-regex" "0.16.2"
                "0vy4c012f82xcg3gs068mq110zhsrnajh58fmq1jxr7vaijhb2wr"))

(define rust-fast-srgb8-1.0.0
  (crate-source "fast-srgb8" "1.0.0"
                "18g6xwwh4gnkyx1352hnvwagpv0n4y98yp2llm8vyvwxh487abnx"))

(define rust-fastbloom-0.14.1
  (crate-source "fastbloom" "0.14.1"
                "1i639bpknnr5vfs8lvc1g3ra30gzmy68mxgax07wcsdy5m238zsf"))

(define rust-faster-hex-0.10.0
  (crate-source "faster-hex" "0.10.0"
                "0wzvv4a1czxfxmh99cza2y0jps97hm3k1j6r6cs816qp5wnsw8vj"))

(define rust-faster-hex-0.9.0
  (crate-source "faster-hex" "0.9.0"
                "10wi4vqbdpkamw4qvra1ijp4as2j7j1zc66g4rdr6h0xv8gb38m2"))

(define rust-fastrand-1.9.0
  (crate-source "fastrand" "1.9.0"
                "1gh12m56265ihdbzh46bhh0jf74i197wm51jg1cw75q7ggi96475"))

(define rust-fastrand-2.0.0
  (crate-source "fastrand" "2.0.0"
                "0r17m5p8ym5pa1f6cp8rix78ggclg6llnw5hxg168cr56wcdr6b9"))

(define rust-fastrand-2.0.1
  (crate-source "fastrand" "2.0.1"
                "19flpv5zbzpf0rk4x77z4zf25in0brg8l7m304d3yrf47qvwxjr5"))

(define rust-fastrand-2.1.0
  (crate-source "fastrand" "2.1.0"
                "06p5d0rxq7by260m4ym9ial0bwgi0v42lrvhl6nm2g7h0h2m3h4z"))

(define rust-fastrand-2.3.0
  (crate-source "fastrand" "2.3.0"
                "1ghiahsw1jd68df895cy5h3gzwk30hndidn3b682zmshpgmrx41p"))

(define rust-fax-0.2.6
  (crate-source "fax" "0.2.6"
                "1ax0jmvsszxd03hj6ga1kyl7gaqcfw0akg2wf0q6gk9pizaffpgh"))

(define rust-fax-derive-0.2.0
  (crate-source "fax_derive" "0.2.0"
                "0zap434zz4xvi5rnysmwzzivig593b4ng15vwzwl7js2nw7s3b50"))

(define rust-fdeflate-0.3.7
  (crate-source "fdeflate" "0.3.7"
                "130ga18vyxbb5idbgi07njymdaavvk6j08yh1dfarm294ssm6s0y"))

(define rust-ff-0.13.0
  (crate-source "ff" "0.13.0"
                "0jcl8yhcs5kbfxfpnrhpkkvnk7s666vly6sgawg3nri9nx215m6y"))

(define rust-fiat-crypto-0.2.9
  (crate-source "fiat-crypto" "0.2.9"
                "07c1vknddv3ak7w89n85ik0g34nzzpms6yb845vrjnv9m4csbpi8"))

(define rust-field-offset-0.3.5
  (crate-source "field-offset" "0.3.5"
                "0dc58qzbfis0i031d741aavvi7crzlb4nvfacg461s7n1y03mkx3"))

(define rust-field-offset-0.3.6
  (crate-source "field-offset" "0.3.6"
                "0zq5sssaa2ckmcmxxbly8qgz3sxpb8g1lwv90sdh1z74qif2gqiq"))

(define rust-file-size-1.0.3
  (crate-source "file-size" "1.0.3"
                "1cyj7067fs7ml8pjrwzjy3qrns3yxaxakf0na1v5fffk0l0z2i4m"))

(define rust-filedescriptor-0.8.2
  (crate-source "filedescriptor" "0.8.2"
                "0vplyh0cw35kzq7smmp2ablq0zsknk5rkvvrywqsqfrchmjxk6bi"))

(define rust-filedescriptor-0.8.3
  (crate-source "filedescriptor" "0.8.3"
                "0bb8qqa9h9sj2mzf09yqxn260qkcqvmhmyrmdjvyxcn94knmh1z4"))

(define rust-filetime-0.2.22
  (crate-source "filetime" "0.2.22"
                "1w1a4zb4ciqjl1chvp9dplbacq07jv97pkdn0pzackbk7vfrw0nl"))

(define rust-filetime-0.2.25
  (crate-source "filetime" "0.2.25"
                "11l5zr86n5sr6g6k6sqldswk0jzklm0q95rzikxcns0yk0p55h1m"))

(define rust-find-msvc-tools-0.1.2
  (crate-source "find-msvc-tools" "0.1.2"
                "0nbrhvk4m04hviiwbqp2jwcv9j2k70x0q2kcvfk51iygvaqp7v8w"))

(define rust-find-msvc-tools-0.1.3
  (crate-source "find-msvc-tools" "0.1.3"
                "1cr47xn14dgkyd5ca2jzfk1apkm3wwqvvglqqhrcx4aidv9gk683"))

(define rust-find-msvc-tools-0.1.4
  (crate-source "find-msvc-tools" "0.1.4"
                "09x1sfinrz86bkm6i2d85lpsfnxn0w797g5zisv1nwhaz1w1h1aj"))

(define rust-find-msvc-tools-0.1.5
  (crate-source "find-msvc-tools" "0.1.5"
                "0i1ql02y37bc7xywkqz10kx002vpz864vc4qq88h1jam190pcc1s"))

(define rust-find-msvc-tools-0.1.7
  (crate-source "find-msvc-tools" "0.1.7"
                "0hbc6fl05nrwcavav0yckdn3jg9b4p7zl349shqmd1lcq33fcjgl"))

(define rust-find-msvc-tools-0.1.8
  (crate-source "find-msvc-tools" "0.1.8"
                "1nv8hn78xphg04l6w7iq1v8lsmmqx6ripbig18qn92m9r2yb14c5"))

(define rust-finl-unicode-1.3.0
  (crate-source "finl_unicode" "1.3.0"
                "0qy1rwjxkqbl6g8ngm2p33y83r4mbfk3l22075yv6vlh4nsp1jcl"))

(define rust-finl-unicode-1.4.0
  (crate-source "finl_unicode" "1.4.0"
                "1md4j32sa8g6y7q9yphpslhhjdjxig1bczkjp8mxccz5lv1xsi4q"))

(define rust-fixedbitset-0.4.2
  (crate-source "fixedbitset" "0.4.2"
                "101v41amgv5n9h4hcghvrbfk5vrncx1jwm35rn5szv4rk55i7rqc"))

(define rust-fixedbitset-0.5.7
  (crate-source "fixedbitset" "0.5.7"
                "16fd3v9d2cms2vddf9xhlm56sz4j0zgrk3d2h6v1l7hx760lwrqx"))

(define rust-flate2-1.0.35
  (crate-source "flate2" "1.0.35"
                "0z6h0wa095wncpfngx75wyhyjnqwld7wax401gsvnzjhzgdbydn9"))

(define rust-flate2-1.1.1
  (crate-source "flate2" "1.1.1"
                "1kpycx57dqpkr3vp53b4nq75p9mflh0smxy8hkys4v4ndvkr5vbw"))

(define rust-flate2-1.1.2
  (crate-source "flate2" "1.1.2"
                "07abz7v50lkdr5fjw8zaw2v8gm2vbppc0f7nqm8x3v3gb6wpsgaa"))

(define rust-flate2-1.1.5
  (crate-source "flate2" "1.1.5"
                "1yrvxgxyg7mzksmmcd9i7vc3023kbv3zhdsf8mkjm8c5ivfkxqxz"))

(define rust-flate2-1.1.7
  (crate-source "flate2" "1.1.7"
                "0xn6vsbwwi96nww8n30snfg982ij25hdjld6w8smgh40p6y2s5d2"))

(define rust-flate2-1.1.8
  (crate-source "flate2" "1.1.8"
                "0sgkq8z9ldz06qxl704sm9akfy2r70zp1ixi4mghl2cqbd3dcxdk"))

(define rust-flex-grow-0.1.0
  (crate-source "flex-grow" "0.1.0"
                "0y7bm1bqa6cxwrj1w2l9ja9qcpaafmbgx28ildpnhq8mn0dsw16m"))

(define rust-float-cmp-0.10.0
  (crate-source "float-cmp" "0.10.0"
                "1n760i3nxd2x0zc7fkxkg3vhvdyfbvzngna006cl9s9jacaz775h"))

(define rust-float-cmp-0.9.0
  (crate-source "float-cmp" "0.9.0"
                "1i799ksbq7fj9rm9m82g1yqgm6xi3jnrmylddmqknmksajylpplq"))

(define rust-float-next-after-1.0.0
  (crate-source "float_next_after" "1.0.0"
                "1s7ikn69b394frihag05b0qcw9i9y04qanlhp5c8sjrw70bcrxwb"))

(define rust-float-ord-0.3.2
  (crate-source "float-ord" "0.3.2"
                "0gbvx6dzz8xvj38bj02cajpqd9p5syxjx9jyqpj8414amr4izs4c"))

(define rust-fluent-uri-0.3.2
  (crate-source "fluent-uri" "0.3.2"
                "1rgp9mv03bifxysy863kf38aagvyl76ca6gd3dcx6iyzjrfvc60r"))

(define rust-fluent-uri-0.4.1
  (crate-source "fluent-uri" "0.4.1"
                "13ij9dqj3hnv8029293i6j7wzr8rjqh15m866mi71bjrhd6sqx5w"))

(define rust-fnv-1.0.7
  (crate-source "fnv" "1.0.7"
                "1hc2mcqha06aibcaza94vbi81j6pr9a1bbxrxjfhc91zin8yr7iz"))

(define rust-foldhash-0.1.5
  (crate-source "foldhash" "0.1.5"
                "1wisr1xlc2bj7hk4rgkcjkz3j2x4dhd1h9lwk7mj8p71qpdgbi6r"))

(define rust-foldhash-0.2.0
  (crate-source "foldhash" "0.2.0"
                "1nvgylb099s11xpfm1kn2wcsql080nqmnhj1l25bp3r2b35j9kkp"))

(define rust-font-kit-0.14.3
  (crate-source "font-kit" "0.14.3"
                "1lzkrbglbf076zwm4lnchn45alsky0nbf9qp5r5lqp9894fn2zic"))

(define rust-font-types-0.10.1
  (crate-source "font-types" "0.10.1"
                "1idqs7pfv9134fzkfykjcg5x34gfjclpqqccln7d9jxv0ks599ir"))

(define rust-font-types-0.7.3
  (crate-source "font-types" "0.7.3"
                "14l4g6rgwfc7yynqif0a3ahfn7wy7fx42s9qvjf430x9bjd1z5xk"))

(define rust-font-types-0.9.0
  (crate-source "font-types" "0.9.0"
                "1dwnd6rwi8c725q0zkwk0qlznwp41zjnvn4040x9501nf7srd982"))

(define rust-font8x8-0.3.1
  (crate-source "font8x8" "0.3.1"
                "0znh67wfv3pyx12619rm8v59fc3ig2ai6pfgqxl855hsf6w8hm47"))

(define rust-fontconfig-parser-0.5.7
  (crate-source "fontconfig-parser" "0.5.7"
                "19xdfsvl9sjha9n1lk3s6bqixcihsmjsd7zf3y90rsd69kagrz61"))

(define rust-fontconfig-parser-0.5.8
  (crate-source "fontconfig-parser" "0.5.8"
                "0ijnbzg31sl6v49g7q2l7sl76hjj8z0hvlsz77cdvm029vi77ixv"))

(define rust-fontdb-0.16.2
  (crate-source "fontdb" "0.16.2"
                "1hqxv3jnh06s4bflrwnb39mi3knllfs4mxm44vsn0gzgqch90adh"))

(define rust-fontdb-0.18.0
  (crate-source "fontdb" "0.18.0"
                "0w3p0ybqgl89p85kxwq7dmm1z8bvqhrl4vaf3pq1sp0kq60sqbp3"))

(define rust-fontdb-0.23.0
  (crate-source "fontdb" "0.23.0"
                "0199vry9x8zn9ix4x4rqvv53dy2ryhy68l53jwr580hj7ndphzj5"))

(define rust-foreign-types-0.3.2
  (crate-source "foreign-types" "0.3.2"
                "1cgk0vyd7r45cj769jym4a6s7vwshvd0z4bqrb92q1fwibmkkwzn"))

(define rust-foreign-types-0.5.0
  (crate-source "foreign-types" "0.5.0"
                "0rfr2zfxnx9rz3292z5nyk8qs2iirznn5ff3rd4vgdwza6mdjdyp"))

(define rust-foreign-types-macros-0.2.3
  (crate-source "foreign-types-macros" "0.2.3"
                "0hjpii8ny6l7h7jpns2cp9589016l8mlrpaigcnayjn9bdc6qp0s"))

(define rust-foreign-types-shared-0.1.1
  (crate-source "foreign-types-shared" "0.1.1"
                "0jxgzd04ra4imjv8jgkmdq59kj8fsz6w4zxsbmlai34h26225c00"))

(define rust-foreign-types-shared-0.3.1
  (crate-source "foreign-types-shared" "0.3.1"
                "0nykdvv41a3d4py61bylmlwjhhvdm0b3bcj9vxhqgxaxnp5ik6ma"))

(define rust-form-urlencoded-1.2.1
  (crate-source "form_urlencoded" "1.2.1"
                "0milh8x7nl4f450s3ddhg57a3flcv6yq8hlkyk6fyr3mcb128dp1"))

(define rust-form-urlencoded-1.2.2
  (crate-source "form_urlencoded" "1.2.2"
                "1kqzb2qn608rxl3dws04zahcklpplkd5r1vpabwga5l50d2v4k6b"))

(define rust-fraction-0.15.3
  (crate-source "fraction" "0.15.3"
                "1rss1021dpzr12xl3dyqyj36ji4vv4f83yy9im039cx1y0zqw58g"))

(define rust-fragile-2.0.1
  (crate-source "fragile" "2.0.1"
                "06g69s9w3hmdnjp5b60ph15v367278mgxy1shijrllarc2pnrp98"))

(define rust-freetype-sys-0.20.1
  ;; TODO: Check bundled sources.
  (crate-source "freetype-sys" "0.20.1"
                "0d5iiv95ap3lwy7b0hxbc8caa9ng1fg3wlwrvb7rld39jrdxqzhf"))

(define rust-from-variants-0.6.0
  (crate-source "from_variants" "0.6.0"
                "1qx4jmwljwmcdfc998ndf7iz8wyg7lmlc3vl3fy812f9lfqiw6i2"))

(define rust-from-variants-1.0.2
  (crate-source "from_variants" "1.0.2"
                "1wmv85523z261vwmx1iqjykf0dp8fvy9kgjxj0c7cs2p427rr1af"))

(define rust-from-variants-impl-0.6.0
  (crate-source "from_variants_impl" "0.6.0"
                "06i4bjjxbq6c4hlx2ly04s64d1972zkskshc2v4xx7n8lfghf23y"))

(define rust-from-variants-impl-1.0.2
  (crate-source "from_variants_impl" "1.0.2"
                "17p6djij0ar0c9dlfnq4dj9bgmq16fcsf3winjr9cv8fm12fd9am"))

(define rust-fs-err-3.2.2
  (crate-source "fs-err" "3.2.2"
                "19slg8hg3gf0g70779gj47dxjn4y5d8hz8b6794mc2bmi7pqrxms"))

(define rust-fs-extra-1.3.0
  (crate-source "fs_extra" "1.3.0"
                "075i25z70j2mz9r7i9p9r521y8xdj81q7skslyb7zhqnnw33fw22"))

(define rust-fs2-0.4.3
  (crate-source "fs2" "0.4.3"
                "04v2hwk7035c088f19mfl5b1lz84gnvv2hv6m935n0hmirszqr4m"))

(define rust-fsevent-sys-4.1.0
  ;; TODO: Check bundled sources.
  (crate-source "fsevent-sys" "4.1.0"
                "1liz67v8b0gcs8r31vxkvm2jzgl9p14i78yfqx81c8sdv817mvkn"))

(define rust-fuchsia-cprng-0.1.1
  (crate-source "fuchsia-cprng" "0.1.1"
                "1fnkqrbz7ixxzsb04bsz9p0zzazanma8znfdqjvh39n14vapfvx0"))

(define rust-fuchsia-zircon-0.3.3
  (crate-source "fuchsia-zircon" "0.3.3"
                "10jxc5ks1x06gpd0xg51kcjrxr35nj6qhx2zlc5n7bmskv3675rf"))

(define rust-fuchsia-zircon-sys-0.3.3
  ;; TODO: Check bundled sources.
  (crate-source "fuchsia-zircon-sys" "0.3.3"
                "19zp2085qsyq2bh1gvcxq1lb8w6v6jj9kbdkhpdjrl95fypakjix"))

(define rust-funty-2.0.0
  (crate-source "funty" "2.0.0"
                "177w048bm0046qlzvp33ag3ghqkqw4ncpzcm5lq36gxf2lla7mg6"))

(define rust-futures-0.1.31
  (crate-source "futures" "0.1.31"
                "0y46qbmhi37dqkch8dlfq5aninqpzqgrr98awkb3rn4fxww1lirs"))

(define rust-futures-0.3.30
  (crate-source "futures" "0.3.30"
                "1c04g14bccmprwsvx2j9m2blhwrynq7vhl151lsvcv4gi0b6jp34"))

(define rust-futures-0.3.31
  (crate-source "futures" "0.3.31"
                "0xh8ddbkm9jy8kc5gbvjp9a4b6rqqxvc8471yb2qaz5wm2qhgg35"))

(define rust-futures-channel-0.3.28
  (crate-source "futures-channel" "0.3.28"
                "1wmm9wm5zjigxz61qkscmxp7c30zp08dy63spjz5pch9gva1hmcm"))

(define rust-futures-channel-0.3.30
  (crate-source "futures-channel" "0.3.30"
                "0y6b7xxqdjm9hlcjpakcg41qfl7lihf6gavk8fyqijsxhvbzgj7a"))

(define rust-futures-channel-0.3.31
  (crate-source "futures-channel" "0.3.31"
                "040vpqpqlbk099razq8lyn74m0f161zd0rp36hciqrwcg2zibzrd"))

(define rust-futures-core-0.3.30
  (crate-source "futures-core" "0.3.30"
                "07aslayrn3lbggj54kci0ishmd1pr367fp7iks7adia1p05miinz"))

(define rust-futures-core-0.3.31
  (crate-source "futures-core" "0.3.31"
                "0gk6yrxgi5ihfanm2y431jadrll00n5ifhnpx090c2f2q1cr1wh5"))

(define rust-futures-executor-0.3.30
  (crate-source "futures-executor" "0.3.30"
                "07dh08gs9vfll2h36kq32q9xd86xm6lyl9xikmmwlkqnmrrgqxm5"))

(define rust-futures-executor-0.3.31
  (crate-source "futures-executor" "0.3.31"
                "17vcci6mdfzx4gbk0wx64chr2f13wwwpvyf3xd5fb1gmjzcx2a0y"))

(define rust-futures-io-0.3.30
  (crate-source "futures-io" "0.3.30"
                "1hgh25isvsr4ybibywhr4dpys8mjnscw4wfxxwca70cn1gi26im4"))

(define rust-futures-io-0.3.31
  (crate-source "futures-io" "0.3.31"
                "1ikmw1yfbgvsychmsihdkwa8a1knank2d9a8dk01mbjar9w1np4y"))

(define rust-futures-lite-1.13.0
  (crate-source "futures-lite" "1.13.0"
                "1kkbqhaib68nzmys2dc8j9fl2bwzf2s91jfk13lb2q3nwhfdbaa9"))

(define rust-futures-lite-2.3.0
  (crate-source "futures-lite" "2.3.0"
                "19gk4my8zhfym6gwnpdjiyv2hw8cc098skkbkhryjdaf0yspwljj"))

(define rust-futures-lite-2.5.0
  (crate-source "futures-lite" "2.5.0"
                "18cii1zgxbm04almisj0ycnmf7nj7qqyvy8x0i8mnl9cmqhhvx6f"))

(define rust-futures-lite-2.6.0
  (crate-source "futures-lite" "2.6.0"
                "0cmmgszlmkwsac9pyw5rfjakmshgx4wmzmlyn6mmjs0jav4axvgm"))

(define rust-futures-lite-2.6.1
  (crate-source "futures-lite" "2.6.1"
                "1ba4dg26sc168vf60b1a23dv1d8rcf3v3ykz2psb7q70kxh113pp"))

(define rust-futures-macro-0.3.30
  (crate-source "futures-macro" "0.3.30"
                "1b49qh9d402y8nka4q6wvvj0c88qq91wbr192mdn5h54nzs0qxc7"))

(define rust-futures-macro-0.3.31
  (crate-source "futures-macro" "0.3.31"
                "0l1n7kqzwwmgiznn0ywdc5i24z72zvh9q1dwps54mimppi7f6bhn"))

(define rust-futures-signals-0.3.34
  (crate-source "futures-signals" "0.3.34"
                "15hnby4hw4z7dc6a2hw1m0anrszb2j3sb6y5yydxdk0d1b2fkavh"))

(define rust-futures-sink-0.3.30
  (crate-source "futures-sink" "0.3.30"
                "1dag8xyyaya8n8mh8smx7x6w2dpmafg2din145v973a3hw7f1f4z"))

(define rust-futures-sink-0.3.31
  (crate-source "futures-sink" "0.3.31"
                "1xyly6naq6aqm52d5rh236snm08kw8zadydwqz8bip70s6vzlxg5"))

(define rust-futures-task-0.3.30
  (crate-source "futures-task" "0.3.30"
                "013h1724454hj8qczp8vvs10qfiqrxr937qsrv6rhii68ahlzn1q"))

(define rust-futures-task-0.3.31
  (crate-source "futures-task" "0.3.31"
                "124rv4n90f5xwfsm9qw6y99755y021cmi5dhzh253s920z77s3zr"))

(define rust-futures-timer-3.0.3
  (crate-source "futures-timer" "3.0.3"
                "094vw8k37djpbwv74bwf2qb7n6v6ghif4myss6smd6hgyajb127j"))

(define rust-futures-util-0.3.30
  (crate-source "futures-util" "0.3.30"
                "0j0xqhcir1zf2dcbpd421kgw6wvsk0rpxflylcysn1rlp3g02r1x"))

(define rust-futures-util-0.3.31
  (crate-source "futures-util" "0.3.31"
                "10aa1ar8bgkgbr4wzxlidkqkcxf77gffyj8j7768h831pcaq784z"))

(define rust-fuzzy-matcher-0.3.7
  (crate-source "fuzzy-matcher" "0.3.7"
                "153csv8rsk2vxagb68kpmiknvdd3bzqj03x805khckck28rllqal"))

(define rust-fxhash-0.2.1
  (crate-source "fxhash" "0.2.1"
                "037mb9ichariqi45xm6mz0b11pa92gj38ba0409z3iz239sns6y3"))

(define rust-gag-1.0.0
  (crate-source "gag" "1.0.0"
                "0wjr02svx7jir7b7r69lpfh3assasmqsz4vivzzzpsb677hvw4x7"))

(define rust-gdk-0.18.0
  (crate-source "gdk" "0.18.0"
                "0iinlc369mrp5aj4fcw5ppb1fx1dxnckd5gwvfy5xdzkvqdhifpm"))

(define rust-gdk-pixbuf-0.18.3
  (crate-source "gdk-pixbuf" "0.18.3"
                "0b68ssdyapvq3bgsna9frabbzhjkvvzz8jld4mxkphr29nvk4vs4"))

(define rust-gdk-pixbuf-0.18.5
  (crate-source "gdk-pixbuf" "0.18.5"
                "1v7svvl0g7zybndmis5inaqqgi1mvcc6s1n8rkb31f5zn3qzbqah"))

(define rust-gdk-pixbuf-0.21.2
  (crate-source "gdk-pixbuf" "0.21.2"
                "03q52c62r2m5xkssld51mba91i7s7f92iz0nfvrm94ankx2n8g1a"))

(define rust-gdk-pixbuf-sys-0.18.0
  ;; TODO: Check bundled sources.
  (crate-source "gdk-pixbuf-sys" "0.18.0"
                "1xya543c4ffd2n7aiwwrdxsyc9casdbasafi6ixcknafckm3k61z"))

(define rust-gdk-pixbuf-sys-0.21.2
  ;; TODO: Check bundled sources.
  (crate-source "gdk-pixbuf-sys" "0.21.2"
                "097ppbqq1nf65i7cpihhx324wyfin2f5y7d02d03p3wbd9xfym1q"))

(define rust-gdk-sys-0.18.0
  ;; TODO: Check bundled sources.
  (crate-source "gdk-sys" "0.18.0"
                "1whznljhqqni5sk1qwazkc75ik5gmc1zh8590cbswv9qndn8bzri"))

(define rust-gdk4-0.10.1
  (crate-source "gdk4" "0.10.1"
                "1w6plimn5brysrifkmhd6846059rmd10z808qm038vn2kmj95qn7"))

(define rust-gdk4-0.7.3
  (crate-source "gdk4" "0.7.3"
                "1xiacc63p73apr033gjrb9dsk0y4yxnsljwfxbwfry41snd03nvy"))

(define rust-gdk4-sys-0.10.1
  ;; TODO: Check bundled sources.
  (crate-source "gdk4-sys" "0.10.1"
                "1krqwiyfj53vf9bxqp3gkaw1skcgvdjrnih4gqmbzq7ili7igwzl"))

(define rust-gdk4-sys-0.7.2
  ;; TODO: Check bundled sources.
  (crate-source "gdk4-sys" "0.7.2"
                "1w7yvir565sjrrw828lss07749hfpfsr19jdjzwivkx36brl7ayv"))

(define rust-generic-array-0.14.7
  (crate-source "generic-array" "0.14.7"
                "16lyyrzrljfq424c3n8kfwkqihlimmsg5nhshbbp48np3yjrqr45"))

(define rust-generic-array-0.14.9
  (crate-source "generic-array" "0.14.9"
                "1wpdn5ngpqkkyyibbg7wa4cfg0y8zjc57spaia2h47jkk0qp9djb"))

(define rust-gensym-0.1.1
  (crate-source "gensym" "0.1.1"
                "10iwpbjapcdg9wjhrsc32dmzr2dcfzvhd30pzi0fmhh6bx6cwgci"))

(define rust-gethostname-0.3.0
  (crate-source "gethostname" "0.3.0"
                "0xy1wbx5k2bzi4cbaqj9wqgqsbn4f8pm6nsm1d86mibk66xd8rdv"))

(define rust-gethostname-0.4.3
  (crate-source "gethostname" "0.4.3"
                "063qqhznyckwx9n4z4xrmdv10s0fi6kbr17r6bi1yjifki2y0xh1"))

(define rust-gethostname-1.1.0
  (crate-source "gethostname" "1.1.0"
                "1n6bj9gh503ggjblfjcai96gmxynxsrykaynljlrfdra34q95m0v"))

(define rust-getrandom-0.2.10
  (crate-source "getrandom" "0.2.10"
                "09zlimhhskzf7cmgcszix05wyz2i6fcpvh711cv1klsxl6r3chdy"))

(define rust-getrandom-0.2.12
  (crate-source "getrandom" "0.2.12"
                "1d8jb9bv38nkwlqqdjcav6gxckgwc9g30pm3qq506rvncpm9400r"))

(define rust-getrandom-0.2.15
  (crate-source "getrandom" "0.2.15"
                "1mzlnrb3dgyd1fb84gvw10pyr8wdqdl4ry4sr64i1s8an66pqmn4"))

(define rust-getrandom-0.2.16
  (crate-source "getrandom" "0.2.16"
                "14l5aaia20cc6cc08xdlhrzmfcylmrnprwnna20lqf746pqzjprk"))

(define rust-getrandom-0.2.17
  (crate-source "getrandom" "0.2.17"
                "1l2ac6jfj9xhpjjgmcx6s1x89bbnw9x6j9258yy6xjkzpq0bqapz"))

(define rust-getrandom-0.2.9
  (crate-source "getrandom" "0.2.9"
                "1r6p47dd9f9cgiwlxmksammbfwnhsv5hjkhd0kjsgnzanad1spn8"))

(define rust-getrandom-0.3.1
  (crate-source "getrandom" "0.3.1"
                "1y154yzby383p63ndw6zpfm0fz3vf6c0zdwc7df6vkl150wrr923"))

(define rust-getrandom-0.3.2
  (crate-source "getrandom" "0.3.2"
                "1w2mlixa1989v7czr68iji7h67yra2pbg3s480wsqjza1r2sizkk"))

(define rust-getrandom-0.3.3
  (crate-source "getrandom" "0.3.3"
                "1x6jl875zp6b2b6qp9ghc84b0l76bvng2lvm8zfcmwjl7rb5w516"))

(define rust-getrandom-0.3.4
  (crate-source "getrandom" "0.3.4"
                "1zbpvpicry9lrbjmkd4msgj3ihff1q92i334chk7pzf46xffz7c9"))

(define rust-gettext-rs-0.7.0
  (crate-source "gettext-rs" "0.7.0"
                "0r7kahqcjrkm83d3gzzkn83fnw2bnqj2ank5z6hsm66izalai7p4"))

(define rust-gettext-sys-0.21.3
  ;; TODO: Check bundled sources.
  (crate-source "gettext-sys" "0.21.3"
                "17c3qdbirxsf9csqzp4z4jaqck2n72z4nw3nh9vhd8jn1zhf4g66"))

(define rust-gif-0.13.1
  (crate-source "gif" "0.13.1"
                "1whrkvdg26gp1r7f95c6800y6ijqw5y0z8rgj6xihpi136dxdciz"))

(define rust-gif-0.13.3
  (crate-source "gif" "0.13.3"
                "06z6gll24q7psbz9fb86jbcbmgwnxkym8jsp0fbq5qikbqilgq2a"))

(define rust-gif-0.14.1
  (crate-source "gif" "0.14.1"
                "0pn3ldqjk0ng1vbc3r3zqqrnjkn6s3f3ndk96lhhrn0q82l2ppzm"))

(define rust-gimli-0.27.2
  (crate-source "gimli" "0.27.2"
                "1d5v6jjchf4872jynjsg5ni4vankm1341bas8qindygb6g9962md"))

(define rust-gimli-0.28.0
  (crate-source "gimli" "0.28.0"
                "1h7hcl3chfvd2gfrrxjymnwj7anqxjslvz20kcargkvsya2dgf3g"))

(define rust-gimli-0.28.1
  (crate-source "gimli" "0.28.1"
                "0lv23wc8rxvmjia3mcxc6hj9vkqnv1bqq0h8nzjcgf71mrxx6wa2"))

(define rust-gimli-0.29.0
  (crate-source "gimli" "0.29.0"
                "1zgzprnjaawmg6zyic4f2q2hc39kdhn116qnkqpgvsasgc3x9v20"))

(define rust-gimli-0.31.1
  (crate-source "gimli" "0.31.1"
                "0gvqc0ramx8szv76jhfd4dms0zyamvlg4whhiz11j34hh3dqxqh7"))

(define rust-gimli-0.32.3
  (crate-source "gimli" "0.32.3"
                "1iqk5xznimn5bfa8jy4h7pa1dv3c624hzgd2dkz8mpgkiswvjag6"))

(define rust-gio-0.18.4
  (crate-source "gio" "0.18.4"
                "0wsc6mnx057s4ailacg99dwgna38dbqli5x7a6y9rdw75x9qzz6l"))

(define rust-gio-0.21.2
  (crate-source "gio" "0.21.2"
                "0hjanww3rz2568r9hz4nh91h11451m44kk6ww8dpg2kl5g0yys7d"))

(define rust-gio-sys-0.18.1
  ;; TODO: Check bundled sources.
  (crate-source "gio-sys" "0.18.1"
                "1lip8z35iy9d184x2qwjxlbxi64q9cpayy7v1p5y9xdsa3w6smip"))

(define rust-gio-sys-0.21.2
  ;; TODO: Check bundled sources.
  (crate-source "gio-sys" "0.21.2"
                "01mp37p1vp5hp9arv1zm2d836b05yazyxnfg13hvnyljvpvd47hp"))

(define rust-git2-0.19.0
  (crate-source "git2" "0.19.0"
                "091pv7866z1qjq800ys0wjv8n73wrv7fqdrddxcnq36w8lzbf0xr"))

(define rust-git2-0.20.1
  (crate-source "git2" "0.20.1"
                "1fgf67h78yrw2gm1n8ghgr0jwsbkvmjfhnbng9zrm2n68jxbh82j"))

(define rust-git2-0.20.2
  (crate-source "git2" "0.20.2"
                "0451zzmvblvlrj6y6pgdsxrqh42hi789n3k9lp0hslmi6fhhgsrd"))

(define rust-gix-0.71.0
  (crate-source "gix" "0.71.0"
                "0dblc4j5bhs973y04zp2sah6xvjizqn6h4pq2agkrz0pd3n727m6"))

(define rust-gix-0.74.1
  (crate-source "gix" "0.74.1"
                "0hsc6k2g2163vjkvgkp3f6kp28xajkw9b134h07bxpk5l7zadlsz"))

(define rust-gix-actor-0.34.0
  (crate-source "gix-actor" "0.34.0"
                "1rr05a5nfwba8nl5zg75ng7j762nn658vfl25ywa9b1881ywhf7l"))

(define rust-gix-actor-0.35.6
  (crate-source "gix-actor" "0.35.6"
                "17m57fpxh8k1frq8dpcylw9vkxialjr8wh9hq16yzdkdwskm2ylq"))

(define rust-gix-attributes-0.25.0
  (crate-source "gix-attributes" "0.25.0"
                "0zyxr1257fzfvi79109z2dmakm40cznmrdpqjq8a22j3w0jmiqp4"))

(define rust-gix-attributes-0.28.1
  (crate-source "gix-attributes" "0.28.1"
                "13d11bskqshv7qfb5c5bcmy9a1vgval7d007vd1zq54kssnr2rfc"))

(define rust-gix-bitmap-0.2.14
  (crate-source "gix-bitmap" "0.2.14"
                "0h3msc00gi2vr2k4q41ddb68qprbvkih824glq6na0lmqrjrgnxi"))

(define rust-gix-bitmap-0.2.15
  (crate-source "gix-bitmap" "0.2.15"
                "0cg5lp3ffdcxgbn5gj5dsdv376l70sspdf0wa9h5hnx7p1hh25ay"))

(define rust-gix-chunk-0.4.11
  (crate-source "gix-chunk" "0.4.11"
                "0vxxq4q5pn5cz2xhghcjpp8z83r8xxy74gsffvf9k1lmcj3is7qb"))

(define rust-gix-chunk-0.4.12
  (crate-source "gix-chunk" "0.4.12"
                "1swf50dk3i9gbq8bsg25hkhj2658261vnlcmazzvcz374lw6ndaw"))

(define rust-gix-command-0.5.0
  (crate-source "gix-command" "0.5.0"
                "1y6yfasb60507mxmpq2bwqpldwzccclzwmz1z2bscwvphjaqjdy0"))

(define rust-gix-command-0.6.3
  (crate-source "gix-command" "0.6.3"
                "0qkfyrk67qzhbwva880iaj5vhwa2yg3kkg3gf2kp4j6wr5kq6p09"))

(define rust-gix-commitgraph-0.27.0
  (crate-source "gix-commitgraph" "0.27.0"
                "1zi819j9ska52pv82rz2q5d36csqq6vkqpwpvd852l57nx4vwg04"))

(define rust-gix-commitgraph-0.30.1
  (crate-source "gix-commitgraph" "0.30.1"
                "1vsijwv3kgz3f7d47lgy3yli0y3i0rslv151sq0gzw81dkzr8sc2"))

(define rust-gix-config-0.44.0
  (crate-source "gix-config" "0.44.0"
                "1ylcdqqkm7ipx4m19x69mnfc3ljmyvvsnj8v4r04jq26yw5q6vww"))

(define rust-gix-config-0.47.1
  (crate-source "gix-config" "0.47.1"
                "1hzvzqj0b22hnp0n8yb11w0g576mwj5lhlyv0y9dw9chm5zgax0y"))

(define rust-gix-config-value-0.14.12
  (crate-source "gix-config-value" "0.14.12"
                "1dj4g52s18ab01pnw55rd0qdf7frdxryzawccy21h56gqi2cihld"))

(define rust-gix-config-value-0.15.3
  (crate-source "gix-config-value" "0.15.3"
                "0r7gb9yww7l9d8yngv28mgmqr5pg0sk283krmp1v0x0w0sxrlj1c"))

(define rust-gix-date-0.10.7
  (crate-source "gix-date" "0.10.7"
                "0w91kzh7xig2df0mpwpi8lz2qmiwhaysmnj48ax1cz5a8p84a4k6"))

(define rust-gix-date-0.9.4
  (crate-source "gix-date" "0.9.4"
                "1r0pc9ra4r7qxwsyd0jvxh3vsnm3jvkgkr19qbxi2dbxxic018ys"))

(define rust-gix-diff-0.51.0
  (crate-source "gix-diff" "0.51.0"
                "1y5301nl772hhhwif1rsrz6w6dp4zcg4si1z4d75xj5gsbd7bjd2"))

(define rust-gix-diff-0.54.1
  (crate-source "gix-diff" "0.54.1"
                "015y190z5zdn3gr66yaxcfap0486fxwirmshlqcs5b0v8bddjy6d"))

(define rust-gix-discover-0.39.0
  (crate-source "gix-discover" "0.39.0"
                "10b7x3pnj3far7zqm728dfm1spqy6k9iiphm75m50m5q951qmyzp"))

(define rust-gix-discover-0.42.0
  (crate-source "gix-discover" "0.42.0"
                "1ihig2kzxzzbnqgd931h644aprj0h8rqibviciik81l1adqm894x"))

(define rust-gix-features-0.41.1
  (crate-source "gix-features" "0.41.1"
                "0d4n21h3z63wfqj2gg69vqqiddwwxgyvs8py412x2n4l45860v81"))

(define rust-gix-features-0.44.1
  (crate-source "gix-features" "0.44.1"
                "0sliz1q51kic6nhlsnpln6v6hf4x7bxmgyq74c83aqaqs69lb9nz"))

(define rust-gix-filter-0.18.0
  (crate-source "gix-filter" "0.18.0"
                "12dbr5x98fm8p9457d627919ng0nk0dxm0pwh8mrpk65znzjnayb"))

(define rust-gix-filter-0.21.0
  (crate-source "gix-filter" "0.21.0"
                "1yxqv5380srjndladw4yqbz2x65r554ir6xgx80xl24q5i2m64hx"))

(define rust-gix-fs-0.14.0
  (crate-source "gix-fs" "0.14.0"
                "04f1z60r0lyi8cp1p09nr9l27wa3jk4fbrakq35ahpyw41hqh7lm"))

(define rust-gix-fs-0.17.0
  (crate-source "gix-fs" "0.17.0"
                "13x2yq3v19mywdlcybmdcpi8sswh2xnkh62dv76cbkaqca4ws7iz"))

(define rust-gix-glob-0.19.0
  (crate-source "gix-glob" "0.19.0"
                "0c506lb1lfa33ajrfxw3cljbgfcmqv85z7h9l9ryfwrlq2cj95r0"))

(define rust-gix-glob-0.22.1
  (crate-source "gix-glob" "0.22.1"
                "0mlxzja9hidv2rwwmvxv5kzjsla9nrd66ixdngyqh2hb2n94j9bl"))

(define rust-gix-hash-0.17.0
  (crate-source "gix-hash" "0.17.0"
                "0jfbdmb1a22x4arhccrbpcwjkkcmfphs3nif6h1qp5b341r7jkl3"))

(define rust-gix-hash-0.20.1
  (crate-source "gix-hash" "0.20.1"
                "1hc5d6856lgxz0c9hqsaqcb9ja6l9k3988qypsq4ang9pslkcq42"))

(define rust-gix-hashtable-0.10.0
  (crate-source "gix-hashtable" "0.10.0"
                "15fixl2wig4qaa9xhykny5qzs5y534sfyzv5l82aa3b4m4z4lzd2"))

(define rust-gix-hashtable-0.8.0
  (crate-source "gix-hashtable" "0.8.0"
                "0jh15sf0r6cg3gava752xi694zidzx8xf7nw3zf8d49af3c6cq7h"))

(define rust-gix-ignore-0.14.0
  (crate-source "gix-ignore" "0.14.0"
                "060x4vqgpjnz0kikarcdq27mfkcjh4znlxbz8p8i0fj91wwch9ws"))

(define rust-gix-ignore-0.17.1
  (crate-source "gix-ignore" "0.17.1"
                "0jgwsywcnll77ch54sr89yid577xmi5nha99gwxi520lk9kskdlk"))

(define rust-gix-index-0.39.0
  (crate-source "gix-index" "0.39.0"
                "19q8p7649hssm0yj3kxllgb8rkm1xqfxb00abnm56d0mskifqnw5"))

(define rust-gix-index-0.42.1
  (crate-source "gix-index" "0.42.1"
                "16dfbyba3g14hhg8mmdy8anpfk1gzkm8sksajqz4zslqzd14a91i"))

(define rust-gix-lock-17.0.0
  (crate-source "gix-lock" "17.0.0"
                "0vqjl0l5f6nbkkw6lwy3rvghnjwfpjgzri8vajshslil3kqvhiyz"))

(define rust-gix-lock-19.0.0
  (crate-source "gix-lock" "19.0.0"
                "0zy6sm6zgq49p8vijg7kca4axmnh47x617f2q0xh4rls89bpi7bj"))

(define rust-gix-object-0.48.0
  (crate-source "gix-object" "0.48.0"
                "16nn1ndxij6sv5fsidsaja192lzdc81ix9wy1j93bhgzwvdgqhs9"))

(define rust-gix-object-0.51.1
  (crate-source "gix-object" "0.51.1"
                "0la033asby8kkhyqln1fgibnyfgvjs1fkj0q4c6whnc7ccaiifl7"))

(define rust-gix-odb-0.68.0
  (crate-source "gix-odb" "0.68.0"
                "0476ynwy0aydycadpfac15xrqa56dq33y44kfmmyp0n9vi06sc2h"))

(define rust-gix-odb-0.71.1
  (crate-source "gix-odb" "0.71.1"
                "0d56y50sh20j1795cwsac5isyq07zfayc6wczq768qpascv6gz3f"))

(define rust-gix-pack-0.58.0
  (crate-source "gix-pack" "0.58.0"
                "1jvrjzrx5q8596cdyp0arnagnxl7zqnd62549ii2cg1r17xzyrcv"))

(define rust-gix-pack-0.61.1
  (crate-source "gix-pack" "0.61.1"
                "0q5jnv7nbjqzyg23r52rf070mh6jb990v8lxfvs78s779d96173i"))

(define rust-gix-packetline-0.18.4
  (crate-source "gix-packetline" "0.18.4"
                "011sdpf03fp066v9q8zcjjz63vwavaqbl1nw84j3bmgl1jkl8f0j"))

(define rust-gix-packetline-0.19.3
  (crate-source "gix-packetline" "0.19.3"
                "1krg4sl658n84yq398fpklbgdk17vmi2grrj16w6mrs8a65nla34"))

(define rust-gix-packetline-blocking-0.18.3
  (crate-source "gix-packetline-blocking" "0.18.3"
                "1crknbdkdnh5aanpyda3dhm2c0a3hacl1h5shxay9iq5w6i3xkqy"))

(define rust-gix-packetline-blocking-0.19.3
  (crate-source "gix-packetline-blocking" "0.19.3"
                "00fhajz5ds0s8jhln942y800vz6cyng9x13xahwcns0yshx9ric9"))

(define rust-gix-path-0.10.15
  (crate-source "gix-path" "0.10.15"
                "11xylymhw8maxv5z81w5hrxry10sibw4vw516pzmaakb5y76c47r"))

(define rust-gix-path-0.10.21
  (crate-source "gix-path" "0.10.21"
                "1f3ipdf56cxklalxr3c2cvnx4jy4h046dc4lkfpr5whgs0fb85h4"))

(define rust-gix-pathspec-0.10.0
  (crate-source "gix-pathspec" "0.10.0"
                "057ch8xlbadf2rnjvlzs5ciqagwn4m8h492b0x4xcrlh7hn45y7y"))

(define rust-gix-pathspec-0.13.0
  (crate-source "gix-pathspec" "0.13.0"
                "0a618ml3ckrmyp7i1fxphb9mnaljmfd8c671vfi6az6agm2jhpnh"))

(define rust-gix-protocol-0.49.0
  (crate-source "gix-protocol" "0.49.0"
                "0njv6mkq6zsf3gw233p8w1r37bw7jcdvw0121v1hp2323npdsy2n"))

(define rust-gix-protocol-0.52.1
  (crate-source "gix-protocol" "0.52.1"
                "00lk7izbck3b3svgza097hxprdnxmsmcrm40fl3zs97rpdrriwb4"))

(define rust-gix-quote-0.5.0
  (crate-source "gix-quote" "0.5.0"
                "0sgr4zw14nnp6l74fc3x3k0a2ik17si41rd59arf6kgq1damq00v"))

(define rust-gix-quote-0.6.1
  (crate-source "gix-quote" "0.6.1"
                "0nsc12pv6ajl6jkw7lxw68z5x5drnv5b0va8mn2nlmminw2fq4p9"))

(define rust-gix-ref-0.51.0
  (crate-source "gix-ref" "0.51.0"
                "12sx86q6gz5y7nrbziqvkfkkxavvcfyp87wn34njvs3wdgmzgqdj"))

(define rust-gix-ref-0.54.1
  (crate-source "gix-ref" "0.54.1"
                "124bxmsmzch085pxvbb5gxs6rrgnyilfj2p68k13knlfy9id50c8"))

(define rust-gix-refspec-0.29.0
  (crate-source "gix-refspec" "0.29.0"
                "1z1g4mfchpxi6xbilx0h7b0pj9v6k5f0r54djglacr123sr8g18x"))

(define rust-gix-refspec-0.32.0
  (crate-source "gix-refspec" "0.32.0"
                "04k9lnmhgzgjbd59l3vamm6xsy4jcy4vf690ny4vm5bnyxh7j54k"))

(define rust-gix-revision-0.33.0
  (crate-source "gix-revision" "0.33.0"
                "1sf3c0c824kj3lbngpygxbj8z5630xinabznml605wwd2m7alb1l"))

(define rust-gix-revision-0.36.1
  (crate-source "gix-revision" "0.36.1"
                "04r4n6j5cnkkgxhcll5nqpgcjd5is941pmg75f2651qdadz2di8k"))

(define rust-gix-revwalk-0.19.0
  (crate-source "gix-revwalk" "0.19.0"
                "03fy5glq01v95cqz908crag9ykqamzj0c08k6n6sphfdwpbw7ird"))

(define rust-gix-revwalk-0.22.0
  (crate-source "gix-revwalk" "0.22.0"
                "1cw3ayazzhr1az4cgy1arvzk1r2z49lzfxrlhzvb24npj57xxqh2"))

(define rust-gix-sec-0.10.12
  (crate-source "gix-sec" "0.10.12"
                "122qvp6sll7hkrpjiwf9ga1dni0gwf6j3zzm6cq2zvz97pqv1bj7"))

(define rust-gix-sec-0.12.2
  (crate-source "gix-sec" "0.12.2"
                "1gm6zwymkb03i64a41xmbci3qa215xskiq7g03qzf54idpnn56ga"))

(define rust-gix-shallow-0.3.0
  (crate-source "gix-shallow" "0.3.0"
                "1hrlyhivjan43743dv18w8hvpvc6w3z9550wl9sjbmg1rym9h1fc"))

(define rust-gix-shallow-0.6.0
  (crate-source "gix-shallow" "0.6.0"
                "08w2q2l77pwzbay0x06srv6w9xqqq9pfiffwxnhgzq8yvf94cdz2"))

(define rust-gix-submodule-0.18.0
  (crate-source "gix-submodule" "0.18.0"
                "18ldqsxh0i11n6vi54bakis3b7yw9rnh2j4mbqv5ql2r4063kivq"))

(define rust-gix-submodule-0.21.0
  (crate-source "gix-submodule" "0.21.0"
                "0mdfyyzd4xn73915vfv643g50a6r5ln64132q17vq2mm6diw1b4v"))

(define rust-gix-tempfile-17.0.0
  (crate-source "gix-tempfile" "17.0.0"
                "16q9gwc2lxrqqpxhvgpw438kgmnj2q7anzww1iax79drpcwy8v9x"))

(define rust-gix-tempfile-19.0.1
  (crate-source "gix-tempfile" "19.0.1"
                "11h0h9pgxmfl429ql6rv39xxmgqy70w41n3r58ir6xp5aimzqrg2"))

(define rust-gix-trace-0.1.12
  (crate-source "gix-trace" "0.1.12"
                "1xv54v5y91vxjx351wl3yk66fwk7ybkna2knbxlnj34j6qh6lfbw"))

(define rust-gix-trace-0.1.15
  (crate-source "gix-trace" "0.1.15"
                "0mrl1ynk9v9zad1wiydrzihqkqgaaiv1lfkb3cwzcd19vsl5jgqx"))

(define rust-gix-transport-0.46.0
  (crate-source "gix-transport" "0.46.0"
                "0zkq877jmmraqz9pflncn386fnr1yakq894x71w85jmzf0l8rxmk"))

(define rust-gix-transport-0.49.1
  (crate-source "gix-transport" "0.49.1"
                "0msrg4z7z3fnmy26lbhi0zyk8svyxy27l331dvib3k1aj9vlmnn8"))

(define rust-gix-traverse-0.45.0
  (crate-source "gix-traverse" "0.45.0"
                "113kzy27skxddmg9qgp84wbjwzshgcpi153604h1pdmxz14v1h1n"))

(define rust-gix-traverse-0.48.0
  (crate-source "gix-traverse" "0.48.0"
                "18p2anshr5r97fpflq5kndsqawaji23n9z9585flz8q3vsx2c8a1"))

(define rust-gix-url-0.30.0
  (crate-source "gix-url" "0.30.0"
                "0q78bb1ark0chavckh5gspsip7d0gbfv02yqfx4vipgijczy5ps8"))

(define rust-gix-url-0.33.1
  (crate-source "gix-url" "0.33.1"
                "1r9q0bjff6j0zdg0xv22p5pzw6ydvnb9chhhxf2m8a6rins0g6y7"))

(define rust-gix-utils-0.2.0
  (crate-source "gix-utils" "0.2.0"
                "1l7qfjkw6lig08d7hb5jvjn5q9fv1717n2zygkapyglhrwj8g7qq"))

(define rust-gix-utils-0.3.1
  (crate-source "gix-utils" "0.3.1"
                "1igpdrs5dxlk5y7hx8c05pkqbv8vf540lxhzb5a2i393n7gxpz5y"))

(define rust-gix-validate-0.10.1
  (crate-source "gix-validate" "0.10.1"
                "1r7xvdhvvf0dl83x8ysf6j5cpzd8f52ysw7qjjjp1s8nnnjn67jv"))

(define rust-gix-validate-0.9.4
  (crate-source "gix-validate" "0.9.4"
                "11204daz5qlk9kqnmiq4syv0n21phkiy3xkwxmwnrnh964jz3d9l"))

(define rust-gix-worktree-0.40.0
  (crate-source "gix-worktree" "0.40.0"
                "0yvjvjs1y6q7qikphvpj85v92sya3pac1b9hxm7jgakr9fy0sxpp"))

(define rust-gix-worktree-0.43.1
  (crate-source "gix-worktree" "0.43.1"
                "013y2fskxz7d4a37jgf0hmy3bg4a907v8my74g4wq3ibnv4dzwwd"))

(define rust-gl-0.14.0
  (crate-source "gl" "0.14.0"
                "015lgy3qpzdw7mnh59a4y4hdjq1fhv7nkqlmh1h6fzc212qxlkm9"))

(define rust-gl-generator-0.14.0
  (crate-source "gl_generator" "0.14.0"
                "0k8j1hmfnff312gy7x1aqjzcm8zxid7ij7dlb8prljib7b1dz58s"))

(define rust-glam-0.25.0
  (crate-source "glam" "0.25.0"
                "1cr80mz1qyc73fvp2d93d4g2svv675fmcrkrzi0bpyajpvcna5hm"))

(define rust-glassbench-0.4.4
  (crate-source "glassbench" "0.4.4"
                "0459fsmfhdvg2z1085a6gnydl1y8s3xanrr9l3vqqd0s51q7hqsa"))

(define rust-glib-0.18.5
  (crate-source "glib" "0.18.5"
                "1r8fw0627nmn19bgk3xpmcfngx3wkn7mcpq5a8ma3risx3valg93"))

(define rust-glib-0.21.3
  (crate-source "glib" "0.21.3"
                "13qcn8cghp35315gbmnwm0jzssq888pmay2q6d4whdkmaz2cpwp1"))

(define rust-glib-macros-0.18.3
  (crate-source "glib-macros" "0.18.3"
                "19crnw5a57w02njpbsmdqwbkncl6hw6g3mv554y8dqzcrri3jybj"))

(define rust-glib-macros-0.18.5
  (crate-source "glib-macros" "0.18.5"
                "1p5cla53fcp195zp0hkqpmnn7iwmkdswhy7xh34002bw8y7j5c0b"))

(define rust-glib-macros-0.21.2
  (crate-source "glib-macros" "0.21.2"
                "17viphsjhpvhscdn52dsdz5csyhk91xs2x6jg1nl5bndxqbakvam"))

(define rust-glib-sys-0.18.1
  ;; TODO: Check bundled sources.
  (crate-source "glib-sys" "0.18.1"
                "164qhsfmlzd5mhyxs8123jzbdfldwxbikfpq5cysj3lddbmy4g06"))

(define rust-glib-sys-0.21.2
  ;; TODO: Check bundled sources.
  (crate-source "glib-sys" "0.21.2"
                "1q97z7ii4qgmyxcz0m7c5xil9n7vbm3b0msfcwhra8zpvl7kv7fh"))

(define rust-glob-0.3.2
  (crate-source "glob" "0.3.2"
                "1cm2w34b5w45fxr522h5b0fv1bxchfswcj560m3pnjbia7asvld8"))

(define rust-glob-0.3.3
  (crate-source "glob" "0.3.3"
                "106jpd3syfzjfj2k70mwm0v436qbx96wig98m4q8x071yrq35hhc"))

(define rust-globset-0.4.15
  (crate-source "globset" "0.4.15"
                "06gv8a5mg5q724lhdq4hp6zmv923whzm7mgpkghz3rs6crlcxw8m"))

(define rust-globset-0.4.18
  (crate-source "globset" "0.4.18"
                "1qsp3wg0mgxzmshcgymdlpivqlc1bihm6133pl6dx2x4af8w3psj"))

(define rust-glow-0.13.1
  (crate-source "glow" "0.13.1"
                "1c91n554dp4bdp5d86rpl77ryv6rjyrqn7735m7mfcivqh28wd5x"))

(define rust-glow-0.16.0
  (crate-source "glow" "0.16.0"
                "022z12nlyfpy36fvp2szq792xix1xbgkznpmicf1c404sxhfmrf5"))

(define rust-glslang-0.6.2
  (crate-source "glslang" "0.6.2"
                "1ad31qpk4xal64i0gmm1y2qxkncfwc50bs80z1dg50fklngypa39"))

(define rust-glslang-sys-0.7.0+1062752
  ;; TODO: Check bundled sources.
  (crate-source "glslang-sys" "0.7.0+1062752"
                "0acxczh29yxd8wr10fpy81gq23708vhr3zd6s24mrnvrki8cm8hk"))

(define rust-glutin-wgl-sys-0.5.0
  ;; TODO: Check bundled sources.
  (crate-source "glutin_wgl_sys" "0.5.0"
                "1b9f6qjc8gwhfxac4fpxkvv524l493f6b6q764nslpwmmjnri03c"))

(define rust-glutin-wgl-sys-0.6.1
  ;; TODO: Check bundled sources.
  (crate-source "glutin_wgl_sys" "0.6.1"
                "0gng2810jb5x133lmy17qifjx6s90lnprm86afg7mfls505y0kic"))

(define rust-gobject-sys-0.18.0
  ;; TODO: Check bundled sources.
  (crate-source "gobject-sys" "0.18.0"
                "0i6fhp3m6vs3wkzyc22rk2cqj68qvgddxmpaai34l72da5xi4l08"))

(define rust-gobject-sys-0.21.2
  ;; TODO: Check bundled sources.
  (crate-source "gobject-sys" "0.21.2"
                "0388b5gx7v72y30q27byilvbq2nnrv72l3vvgq8fqwv1fzc433jk"))

(define rust-goblin-0.10.4
  (crate-source "goblin" "0.10.4"
                "1g94h674qic35zh8k3px1jlbvpw4bq79i333cpr82vvfaj67bdjd"))

(define rust-gpg-error-0.6.2
  (crate-source "gpg-error" "0.6.2"
                "06frmsf3sn286snvwxlwq5jyfvg8wq2661y877b38mz9s0aawnjl"))

(define rust-gpgme-0.11.0
  (crate-source "gpgme" "0.11.0"
                "0ky4b816g1c7f4r82jbsmfivrl0f8wmvfd27k3dsr3p5zcr9flsp"))

(define rust-gpgme-sys-0.11.0
  ;; TODO: Check bundled sources.
  (crate-source "gpgme-sys" "0.11.0"
                "1xqwfilq63nmxk38cz4i64nz08vpj6ndcdwl48k4lvn0b7b274jh"))

(define rust-gpu-alloc-0.6.0
  (crate-source "gpu-alloc" "0.6.0"
                "0wd1wq7qs8ja0cp37ajm9p1r526sp6w0kvjp3xx24jsrjfx2vkgv"))

(define rust-gpu-alloc-types-0.3.0
  (crate-source "gpu-alloc-types" "0.3.0"
                "190wxsp9q8c59xybkfrlzqqyrxj6z39zamadk1q7v0xad2s07zwq"))

(define rust-gpu-allocator-0.25.0
  (crate-source "gpu-allocator" "0.25.0"
                "11484bhn0p8555gprr58kmz1aqccz1zqbx5ww4c3rl38i4qzcmkg"))

(define rust-gpu-allocator-0.27.0
  (crate-source "gpu-allocator" "0.27.0"
                "1kfa6kvvslmrr63p4n5fsm8myp03xi5lzylywys9f0l0xyjs4lf1"))

(define rust-gpu-descriptor-0.2.4
  (crate-source "gpu-descriptor" "0.2.4"
                "0b38pi460ajx8ksb61zxardwkpa27qgz8fpm252mczlfrqddy4fc"))

(define rust-gpu-descriptor-0.3.2
  (crate-source "gpu-descriptor" "0.3.2"
                "1jm0acxkw9lrzzcbvjqynwdr53qsqz7vx5d8c8h77qq5j4s8775q"))

(define rust-gpu-descriptor-types-0.1.2
  (crate-source "gpu-descriptor-types" "0.1.2"
                "135pp1b3bzyr7bfnb30rf9pkgy61h75w0jabi8fpw2q9dxpb7w3b"))

(define rust-gpu-descriptor-types-0.2.0
  (crate-source "gpu-descriptor-types" "0.2.0"
                "14ab90klss7w0ybj95fcnqxjsjya17xjhf576dpvi4zq5ml45wpx"))

(define rust-graphene-rs-0.18.1
  (crate-source "graphene-rs" "0.18.1"
                "00f4q1ra4haap5i7lazwhkdgnb49fs8adk2nm6ki6mjhl76jh8iv"))

(define rust-graphene-rs-0.21.2
  (crate-source "graphene-rs" "0.21.2"
                "1hyx8mm7irx09l9q84snddk8m1q56x1f6fffpzrmp59vbnprlx77"))

(define rust-graphene-sys-0.18.1
  ;; TODO: Check bundled sources.
  (crate-source "graphene-sys" "0.18.1"
                "0n8zlg7z26lwpnvlqp1hjlgrs671skqwagdpm7r8i1zwx3748hfc"))

(define rust-graphene-sys-0.21.2
  ;; TODO: Check bundled sources.
  (crate-source "graphene-sys" "0.21.2"
                "0s553zjb0mgispg2nhbkjlzifmn3560ag44qa1va540ahppbl2i5"))

(define rust-group-0.13.0
  (crate-source "group" "0.13.0"
                "0qqs2p5vqnv3zvq9mfjkmw3qlvgqb0c3cm6p33srkh7pc9sfzygh"))

(define rust-gsk4-0.10.1
  (crate-source "gsk4" "0.10.1"
                "0bgvn6h7amx2hi7xybyp2fa0jkciyzx0qh3nqc0c12fajagpws5n"))

(define rust-gsk4-0.7.3
  (crate-source "gsk4" "0.7.3"
                "0zhzs2dkgiinhgc11akpn2harq3x5n1iq21dnc4h689g3lsqx58d"))

(define rust-gsk4-sys-0.10.1
  ;; TODO: Check bundled sources.
  (crate-source "gsk4-sys" "0.10.1"
                "0llck337fg8gabd4j635gg0w9jf01i05px2ijvqlcj4w9pvbqxjy"))

(define rust-gsk4-sys-0.7.3
  ;; TODO: Check bundled sources.
  (crate-source "gsk4-sys" "0.7.3"
                "0mbdlm9qi1hql48rr29vsj9vlqwc7gxg67wg1q19z67azwz9xg8j"))

(define rust-gtk-0.18.1
  (crate-source "gtk" "0.18.1"
                "0349cpwasirgxqgsf3md0m5p9nixzsknvw55643f2q0bwbhgbi4k"))

(define rust-gtk-layer-shell-0.8.1
  (crate-source "gtk-layer-shell" "0.8.1"
                "10vk23j43wq29isjnd2y752l7zqca8wa642abbdcsm8b0x1idd5d"))

(define rust-gtk-layer-shell-sys-0.7.0
  ;; TODO: Check bundled sources.
  (crate-source "gtk-layer-shell-sys" "0.7.0"
                "0j749cyq2pbyfdjxpzf27wbm2sp2gpac6g28n8q6d4kwmalnzr4h"))

(define rust-gtk-sys-0.18.0
  ;; TODO: Check bundled sources.
  (crate-source "gtk-sys" "0.18.0"
                "08kpdl7ldj8lyv4dyhp9pdk61qj8yyyha5f1jssc1hg23nzkf53p"))

(define rust-gtk3-macros-0.18.0
  (crate-source "gtk3-macros" "0.18.0"
                "0pnpp8bddisgyd1m39xhrw6kd9lawqd2xxvxzdl2jn6vcgxkw1n6"))

(define rust-gtk4-0.10.1
  (crate-source "gtk4" "0.10.1"
                "12iv0as5j26nz49zqrqpsp5dm5qlqsi11221barfvzzf1kp8fy4g"))

(define rust-gtk4-0.7.3
  (crate-source "gtk4" "0.7.3"
                "0hh8nzglmz94v1m1h6vy8z12m6fr7ia467ry0md5fa4p7sm53sss"))

(define rust-gtk4-layer-shell-0.6.3
  (crate-source "gtk4-layer-shell" "0.6.3"
                "1ljxjphync7g6kfrfk9x0q9ksfbf2f0ajjbj45lp8c2yv9yvmm8p"))

(define rust-gtk4-layer-shell-sys-0.4.0
  ;; TODO: Check bundled sources.
  (crate-source "gtk4-layer-shell-sys" "0.4.0"
                "0c8rps2svsl0zalfcanh0kg6dlrs3gmrgpbd82ii0ina922j5rc9"))

(define rust-gtk4-macros-0.10.1
  (crate-source "gtk4-macros" "0.10.1"
                "0sp0qp96jj6xajnblz989k8bwiha5ml2734193blwzkyy6s604c2"))

(define rust-gtk4-macros-0.7.2
  (crate-source "gtk4-macros" "0.7.2"
                "0bw3cchiycf7dw1bw4p8946gv38azxy05a5w0ndgcmxnz6fc8znm"))

(define rust-gtk4-sys-0.10.1
  ;; TODO: Check bundled sources.
  (crate-source "gtk4-sys" "0.10.1"
                "04d0qlf6jvxnd4a6f00k9r8lxmlr6as23jvqzxx5p9csgnpwnx6j"))

(define rust-gtk4-sys-0.7.3
  ;; TODO: Check bundled sources.
  (crate-source "gtk4-sys" "0.7.3"
                "1f2ylskyqkjdik9fij2m46pra4jagnif5xyalbxfk3334fmc9n2l"))

(define rust-guillotiere-0.6.2
  (crate-source "guillotiere" "0.6.2"
                "10m7fhp5kzf09kz08k6apkbzblriyqynjl1wwa9i7jrnq1jmhbdn"))

(define rust-h2-0.3.26
  (crate-source "h2" "0.3.26"
                "1s7msnfv7xprzs6xzfj5sg6p8bjcdpcqcmjjbkd345cyi1x55zl1"))

(define rust-h2-0.3.27
  (crate-source "h2" "0.3.27"
                "0b92141hilij015av6i5ziw9xfx4py3lbjy17yc35z5ih01sbv0b"))

(define rust-h2-0.4.12
  (crate-source "h2" "0.4.12"
                "11hk5mpid8757z6n3v18jwb62ikffrgzjlrgpzqvkqdlzjfbdh7k"))

(define rust-h2-0.4.13
  (crate-source "h2" "0.4.13"
                "0m6w5gg0n0m1m5915bxrv8n4rlazhx5icknkslz719jhh4xdli1g"))

(define rust-h2-0.4.4
  (crate-source "h2" "0.4.4"
                "0sc0ymhiqp4hbz39d405cjbga77wnz2pprbgyc498xs58hlwfvl1"))

(define rust-h2-0.4.7
  (crate-source "h2" "0.4.7"
                "0bljg66n2x3c5yzbi12v2jfcj77hb35rjq0gq21x0d6n52bjgbnc"))

(define rust-h2-0.4.8
  (crate-source "h2" "0.4.8"
                "1hp3lijg1br982kzgglb5ks2ibg68a76z3rl052r8c5vyi7jj5sh"))

(define rust-h3-0.0.8
  (crate-source "h3" "0.0.8"
                "1gmkarh037aaxhmncycsxcjszmkidbfghz6wd4dq4amhrxajp1qh"))

(define rust-h3-quinn-0.0.10
  (crate-source "h3-quinn" "0.0.10"
                "1d7qqjzvqzxb2dr216kvajzjy105nnd4gj1scqqlg9wiiln76blb"))

(define rust-half-2.4.1
  (crate-source "half" "2.4.1"
                "123q4zzw1x4309961i69igzd1wb7pj04aaii3kwasrz3599qrl3d"))

(define rust-half-2.6.0
  (crate-source "half" "2.6.0"
                "1j83v0xaqvrw50ppn0g33zig0zsbdi7xiqbzgn7sd5al57nrd4a5"))

(define rust-half-2.7.1
  (crate-source "half" "2.7.1"
                "0jyq42xfa6sghc397mx84av7fayd4xfxr4jahsqv90lmjr5xi8kf"))

(define rust-halfbrown-0.2.5
  (crate-source "halfbrown" "0.2.5"
                "0vzg46039pd730vc2hdhl09h86j4cd007awwlrf8l407hqd6d245"))

(define rust-halfbrown-0.3.0
  (crate-source "halfbrown" "0.3.0"
                "1airrrxwmqgnpl02cy0xwhscvlnv74qdjwxnpf0d23zpdmf3hb5a"))

(define rust-hash32-0.3.1
  (crate-source "hash32" "0.3.1"
                "01h68z8qi5gl9lnr17nz10lay8wjiidyjdyd60kqx8ibj090pmj7"))

(define rust-hashbrown-0.12.3
  (crate-source "hashbrown" "0.12.3"
                "1268ka4750pyg2pbgsr43f0289l5zah4arir2k4igx5a8c6fg7la"))

(define rust-hashbrown-0.14.1
  (crate-source "hashbrown" "0.14.1"
                "04hdcir5sqgw9318c3sjnhf3jid1mq5ipy0ma2pawpgm28madzbx"))

(define rust-hashbrown-0.14.3
  (crate-source "hashbrown" "0.14.3"
                "012nywlg0lj9kwanh69my5x67vjlfmzfi9a0rq4qvis2j8fil3r9"))

(define rust-hashbrown-0.14.5
  (crate-source "hashbrown" "0.14.5"
                "1wa1vy1xs3mp11bn3z9dv0jricgr6a2j0zkf1g19yz3vw4il89z5"))

(define rust-hashbrown-0.15.2
  (crate-source "hashbrown" "0.15.2"
                "12dj0yfn59p3kh3679ac0w1fagvzf4z2zp87a13gbbqbzw0185dz"))

(define rust-hashbrown-0.15.5
  (crate-source "hashbrown" "0.15.5"
                "189qaczmjxnikm9db748xyhiw04kpmhm9xj9k9hg0sgx7pjwyacj"))

(define rust-hashbrown-0.16.0
  (crate-source "hashbrown" "0.16.0"
                "13blh9j2yv77a6ni236ixiwdzbc1sh2bc4bdpaz7y859yv2bs6al"))

(define rust-hashbrown-0.16.1
  (crate-source "hashbrown" "0.16.1"
                "004i3njw38ji3bzdp9z178ba9x3k0c1pgy8x69pj7yfppv4iq7c4"))

(define rust-hashify-0.2.7
  (crate-source "hashify" "0.2.7"
                "0k2x1vrkafnhhwk5y8yh6a835f81fizwpqygak9nm8mm1slkx7hl"))

(define rust-hashlink-0.10.0
  (crate-source "hashlink" "0.10.0"
                "1h8lzvnl9qxi3zyagivzz2p1hp6shgddfmccyf6jv7s1cdicz0kk"))

(define rust-hashlink-0.9.1
  (crate-source "hashlink" "0.9.1"
                "1byq4nyrflm5s6wdx5qwp96l1qbp2d0nljvrr5yqrsfy51qzz93b"))

(define rust-hassle-rs-0.11.0
  (crate-source "hassle-rs" "0.11.0"
                "147886vviw14zm2a3yh8gs1r81r5gy88lrpv67d4hk7kw5rpwamg"))

(define rust-heapless-0.8.0
  (crate-source "heapless" "0.8.0"
                "1b9zpdjv4qkl2511s2c80fz16fx9in4m9qkhbaa8j73032v9xyqb"))

(define rust-heck-0.3.3
  (crate-source "heck" "0.3.3"
                "0b0kkr790p66lvzn9nsmfjvydrbmh9z5gb664jchwgw64vxiwqkd"))

(define rust-heck-0.4.1
  (crate-source "heck" "0.4.1"
                "1a7mqsnycv5z4z5vnv1k34548jzmc0ajic7c1j8jsaspnhw5ql4m"))

(define rust-heck-0.5.0
  (crate-source "heck" "0.5.0"
                "1sjmpsdl8czyh9ywl3qcsfsq9a307dg4ni2vnlwgnzzqhc4y0113"))

(define rust-heh-0.6.3
  (crate-source "heh" "0.6.3"
                "1zymx14hcb66k31aghb8l8y8q90azkvf4kx67cg9vhsz3scik1in"))

(define rust-hermit-abi-0.2.6
  (crate-source "hermit-abi" "0.2.6"
                "1iz439yz9qzk3rh9pqx2rz5c4107v3qbd7bppfsbzb1mzr02clgf"))

(define rust-hermit-abi-0.3.5
  (crate-source "hermit-abi" "0.3.5"
                "1hw2bxkzyvr0rbnpj0lkasi8h8qf3lyb63hp760cn22fjqaj3inh"))

(define rust-hermit-abi-0.3.9
  (crate-source "hermit-abi" "0.3.9"
                "092hxjbjnq5fmz66grd9plxd0sh6ssg5fhgwwwqbrzgzkjwdycfj"))

(define rust-hermit-abi-0.4.0
  (crate-source "hermit-abi" "0.4.0"
                "1k1zwllx6nfq417hy38x4akw1ivlv68ymvnzyxs76ffgsqcskxpv"))

(define rust-hermit-abi-0.5.1
  (crate-source "hermit-abi" "0.5.1"
                "026bh0y8gpfd62gjm7gx6nyf6bgdyxdn0jc67i1ysl37hm3cwm7i"))

(define rust-hermit-abi-0.5.2
  (crate-source "hermit-abi" "0.5.2"
                "1744vaqkczpwncfy960j2hxrbjl1q01csm84jpd9dajbdr2yy3zw"))

(define rust-hex-0.4.3
  (crate-source "hex" "0.4.3"
                "0w1a4davm1lgzpamwnba907aysmlrnygbqmfis2mqjx5m552a93z"))

(define rust-hexf-parse-0.2.1
  (crate-source "hexf-parse" "0.2.1"
                "1pr3a3sk66ddxdyxdxac7q6qaqjcn28v0njy22ghdpfn78l8d9nz"))

(define rust-hickory-proto-0.24.2
  (crate-source "hickory-proto" "0.24.2"
                "1df1gg333sgjicmf8nbsxlhffp18kdfwcvdgaald1fdgp36zsyj4"))

(define rust-hickory-proto-0.25.2
  (crate-source "hickory-proto" "0.25.2"
                "00k5dk572p0bsrnvsqv1yi8mwfpgwyj7q8bgiacri083q1bgx9pq"))

(define rust-hickory-resolver-0.24.2
  (crate-source "hickory-resolver" "0.24.2"
                "1d243gxb2c2nlm73c2vb4yp908z8mi6ixkriglkfb71qkjx2lbha"))

(define rust-hickory-resolver-0.25.2
  (crate-source "hickory-resolver" "0.25.2"
                "0ypqxipl0dmggv442qn1xxh11lsjr65219wmmg949yqbkflsjqnw"))

(define rust-hkdf-0.12.4
  (crate-source "hkdf" "0.12.4"
                "1xxxzcarz151p1b858yn5skmhyrvn8fs4ivx5km3i1kjmnr8wpvv"))

(define rust-hmac-0.12.1
  (crate-source "hmac" "0.12.1"
                "0pmbr069sfg76z7wsssfk5ddcqd9ncp79fyz6zcm6yn115yc6jbc"))

(define rust-home-0.5.11
  (crate-source "home" "0.5.11"
                "1kxb4k87a9sayr8jipr7nq9wpgmjk4hk4047hmf9kc24692k75aq"))

(define rust-home-0.5.12
  (crate-source "home" "0.5.12"
                "13bjyzgx6q9srnfvl43dvmhn93qc8mh5w7cylk2g13sj3i3pyqnc"))

(define rust-hostname-0.3.1
  (crate-source "hostname" "0.3.1"
                "0rz8yf70cvzl3nry71m4bz9w6x4j9kdz3qng6pnwhk2h20z1qwrw"))

(define rust-http-0.2.12
  (crate-source "http" "0.2.12"
                "1w81s4bcbmcj9bjp7mllm8jlz6b31wzvirz8bgpzbqkpwmbvn730"))

(define rust-http-1.1.0
  (crate-source "http" "1.1.0"
                "0n426lmcxas6h75c2cp25m933pswlrfjz10v91vc62vib2sdvf91"))

(define rust-http-1.2.0
  (crate-source "http" "1.2.0"
                "1skglzdf98j5nzxlii540n11is0w4l80mi5sm3xrj716asps4v7i"))

(define rust-http-1.3.1
  (crate-source "http" "1.3.1"
                "0r95i5h7dr1xadp1ac9453w0s62s27hzkam356nyx2d9mqqmva7l"))

(define rust-http-1.4.0
  (crate-source "http" "1.4.0"
                "06iind4cwsj1d6q8c2xgq8i2wka4ps74kmws24gsi1bzdlw2mfp3"))

(define rust-http-body-0.4.6
  (crate-source "http-body" "0.4.6"
                "1lmyjfk6bqk6k9gkn1dxq770sb78pqbqshga241hr5p995bb5skw"))

(define rust-http-body-1.0.0
  (crate-source "http-body" "1.0.0"
                "0hyn8n3iadrbwq8y0p1rl1275s4nm49bllw5wji29g4aa3dqbb0w"))

(define rust-http-body-1.0.1
  (crate-source "http-body" "1.0.1"
                "111ir5k2b9ihz5nr9cz7cwm7fnydca7dx4hc7vr16scfzghxrzhy"))

(define rust-http-body-util-0.1.1
  (crate-source "http-body-util" "0.1.1"
                "07agldas2qgcfc05ckiarlmf9vzragbda823nqhrqrc6mjrghx84"))

(define rust-http-body-util-0.1.2
  (crate-source "http-body-util" "0.1.2"
                "0kslwazg4400qnc2azkrgqqci0fppv12waicnsy5d8hncvbjjd3r"))

(define rust-http-body-util-0.1.3
  (crate-source "http-body-util" "0.1.3"
                "0jm6jv4gxsnlsi1kzdyffjrj8cfr3zninnxpw73mvkxy4qzdj8dh"))

(define rust-http-lib-0.1.0
  (crate-source "http-lib" "0.1.0"
                "0f0klsaiczfw3rkj5h8zncwg70qkwjqxkjfi45ccnl85r4ivsk4r"))

(define rust-http-mitm-proxy-0.16.0
  (crate-source "http-mitm-proxy" "0.16.0"
                "1ydmnp6ljcy08gb1bjak2qpclv47i0dgzkxy6ragz1zfri4vp56s"))

(define rust-httparse-1.10.1
  (crate-source "httparse" "1.10.1"
                "11ycd554bw2dkgw0q61xsa7a4jn1wb1xbfacmf3dbwsikvkkvgvd"))

(define rust-httparse-1.8.0
  (crate-source "httparse" "1.8.0"
                "010rrfahm1jss3p022fqf3j3jmm72vhn4iqhykahb9ynpaag75yq"))

(define rust-httparse-1.9.4
  (crate-source "httparse" "1.9.4"
                "1nc2s1pziq5ncl39xm7ybdhpnw5xsm505smqirr0py2v2550pk0g"))

(define rust-httparse-1.9.5
  (crate-source "httparse" "1.9.5"
                "0ip9v8m9lvgvq1lznl31wvn0ch1v254na7lhid9p29yx9rbx6wbx"))

(define rust-httpdate-1.0.3
  (crate-source "httpdate" "1.0.3"
                "1aa9rd2sac0zhjqh24c9xvir96g188zldkx0hr6dnnlx5904cfyz"))

(define rust-humantime-2.1.0
  (crate-source "humantime" "2.1.0"
                "1r55pfkkf5v0ji1x6izrjwdq9v6sc7bv99xj6srywcar37xmnfls"))

(define rust-humantime-2.3.0
  (crate-source "humantime" "2.3.0"
                "092lpipp32ayz4kyyn4k3vz59j9blng36wprm5by0g2ykqr14nqk"))

(define rust-hyper-0.14.29
  (crate-source "hyper" "0.14.29"
                "0csbcdsnffb2fwvp12m51m0kzvnmvxn781s31li1la09y7icsqgk"))

(define rust-hyper-1.2.0
  (crate-source "hyper" "1.2.0"
                "0fi6k7hz5fmdph0a5r8hw50d7h2n9zxkizmafcmb65f67bblhr8q"))

(define rust-hyper-1.4.1
  (crate-source "hyper" "1.4.1"
                "01ds8i3q6hw5kw56mavy544m11gkr87zi999siigdl3n1qpd5psh"))

(define rust-hyper-1.5.2
  (crate-source "hyper" "1.5.2"
                "1q7akfb443yrjzkmnnbp2vs8zi15hgbk466rr4y144v4ppabhvr5"))

(define rust-hyper-1.6.0
  (crate-source "hyper" "1.6.0"
                "103ggny2k31z0iq2gzwk2vbx601wx6xkpjpxn40hr3p3b0b5fayc"))

(define rust-hyper-1.7.0
  (crate-source "hyper" "1.7.0"
                "07n59pxzlq621z611cbpvh7p4h9h15v0r7m5wgxygpx02d5aafpb"))

(define rust-hyper-1.8.1
  (crate-source "hyper" "1.8.1"
                "04cxr8j5y86bhxxlyqb8xkxjskpajk7cxwfzzk4v3my3a3rd9cia"))

(define rust-hyper-rustls-0.26.0
  (crate-source "hyper-rustls" "0.26.0"
                "0b4m1jvs147hxi8677n2dxxib663s7c31xmfni7b5qkanihsggm0"))

(define rust-hyper-rustls-0.27.2
  (crate-source "hyper-rustls" "0.27.2"
                "0ma1wyfnqnkz7zyr7wpply3xfvlijd0rqqhb6ajs28c9jhnbxr2y"))

(define rust-hyper-rustls-0.27.5
  (crate-source "hyper-rustls" "0.27.5"
                "1cjr3yf3x5mr3194llsfibacl6j7n2dknii2dwjha4ysyf1ia69d"))

(define rust-hyper-rustls-0.27.7
  (crate-source "hyper-rustls" "0.27.7"
                "0n6g8998szbzhnvcs1b7ibn745grxiqmlpg53xz206v826v3xjg3"))

(define rust-hyper-tls-0.5.0
  (crate-source "hyper-tls" "0.5.0"
                "01crgy13102iagakf6q4mb75dprzr7ps1gj0l5hxm1cvm7gks66n"))

(define rust-hyper-tls-0.6.0
  (crate-source "hyper-tls" "0.6.0"
                "1q36x2yps6hhvxq5r7mc8ph9zz6xlb573gx0x3yskb0fi736y83h"))

(define rust-hyper-util-0.1.10
  (crate-source "hyper-util" "0.1.10"
                "1d1iwrkysjhq63pg54zk3vfby1j7zmxzm9zzyfr4lwvp0szcybfz"))

(define rust-hyper-util-0.1.13
  (crate-source "hyper-util" "0.1.13"
                "1s06md3mq6v6w2zqq0qfag2hw8drsvmxpiqd4mwcl7njnfv97hmi"))

(define rust-hyper-util-0.1.17
  (crate-source "hyper-util" "0.1.17"
                "1a5fcnz0alrg4lx9xf6ja66ihaab58jnm5msnky804wg39cras9w"))

(define rust-hyper-util-0.1.19
  (crate-source "hyper-util" "0.1.19"
                "0pyzc8378baf996l5ycl4y0s3skhxc4z4vkah9mvff3r1vb0ay3j"))

(define rust-hyper-util-0.1.3
  (crate-source "hyper-util" "0.1.3"
                "1akngan7j0n2n0wd25c6952mvqbkj9gp1lcwzyxjc0d37l8yyf6a"))

(define rust-hyper-util-0.1.7
  (crate-source "hyper-util" "0.1.7"
                "1fg9h591skksq5zxnffyisj7487jhdcgj6c7bvlkckn535bhbryd"))

(define rust-hyperlocal-0.9.1
  (crate-source "hyperlocal" "0.9.1"
                "1iy8rhsap5iyigj6s86nk449zl5bahjycy2mswy6nlllp7imqv4q"))

(define rust-hyprland-0.4.0-alpha.2
  (crate-source "hyprland" "0.4.0-alpha.2"
                "0xl21prdpin45iq57rp6cf4cmgy0p0fv4jvs9daz529kzc3cs9yn"))

(define rust-hyprland-0.4.0-beta.3
  (crate-source "hyprland" "0.4.0-beta.3"
                "1agy2gr4qr1anzzqdaxncsiax9ivi6hhvip5j6pr51ap5w2j9z32"))

(define rust-hyprland-macros-0.4.0-alpha.1
  (crate-source "hyprland-macros" "0.4.0-alpha.1"
                "13n3zgrfz6l6hi0g35zy8qkimray89i95x0qb547xrrc316cxn2x"))

(define rust-hyprland-macros-0.4.0-beta.3
  (crate-source "hyprland-macros" "0.4.0-beta.3"
                "044cmwkfyvppdfzjd3qh3g0p06k9jrkdnjg5sw64pbgvrrn7w59i"))

(define rust-i2c-0.1.0
  (crate-source "i2c" "0.1.0"
                "16nfl82c2759w5fbabzjnzlgsivsivcxg84lrzyqbadksyyvgiv0"))

(define rust-i2c-linux-0.1.2
  (crate-source "i2c-linux" "0.1.2"
                "196jgnw9hpmgiy6cd1qdkgji0iznvjz5x1y2sqhi41xa3a3ql9n0"))

(define rust-i2c-linux-sys-0.2.1
  ;; TODO: Check bundled sources.
  (crate-source "i2c-linux-sys" "0.2.1"
                "1y5w7g6sm4fkydcqwfhz1plls22q04xs5lsfvb9j2rh1s070dkam"))

(define rust-iana-time-zone-0.1.60
  (crate-source "iana-time-zone" "0.1.60"
                "0hdid5xz3jznm04lysjm3vi93h3c523w0hcc3xba47jl3ddbpzz7"))

(define rust-iana-time-zone-0.1.61
  (crate-source "iana-time-zone" "0.1.61"
                "085jjsls330yj1fnwykfzmb2f10zp6l7w4fhq81ng81574ghhpi3"))

(define rust-iana-time-zone-0.1.63
  (crate-source "iana-time-zone" "0.1.63"
                "1n171f5lbc7bryzmp1h30zw86zbvl5480aq02z92lcdwvvjikjdh"))

(define rust-iana-time-zone-0.1.64
  (crate-source "iana-time-zone" "0.1.64"
                "1yz980fmhaq9bdkasz35z63az37ci6kzzfhya83kgdqba61pzr9k"))

(define rust-iana-time-zone-haiku-0.1.2
  (crate-source "iana-time-zone-haiku" "0.1.2"
                "17r6jmj31chn7xs9698r122mapq85mfnv98bb4pg6spm0si2f67k"))

(define rust-iced-0.13.1
  (crate-source "iced" "0.13.1"
                "0gy7c0fkn2cnybvrnah2g5nn4151lgzmfd6yksppw1zchjygmb48"))

(define rust-iced-core-0.13.2
  (crate-source "iced_core" "0.13.2"
                "074a9jp49h4rf3wjzck7vi06a688m0ijlwziz0dn952l4wwa44q0"))

(define rust-iced-futures-0.13.2
  (crate-source "iced_futures" "0.13.2"
                "0z2x1ppamdww1641iffq9i7sllyq03yk9q01rwr0zs52bdsac10c"))

(define rust-iced-glyphon-0.6.0
  (crate-source "iced_glyphon" "0.6.0"
                "10nyvdd8j5nblwz2c33wcmakl8rxwgn9828xpi9a4342y5bbphs1"))

(define rust-iced-graphics-0.13.0
  (crate-source "iced_graphics" "0.13.0"
                "0vy6nrkrfwq9ihb051zsh0bg5pycfd7v7qd7mihc2p3dza6a29ds"))

(define rust-iced-renderer-0.13.0
  (crate-source "iced_renderer" "0.13.0"
                "0akcyi214gnay0y046i0fp73iy729q24whxzy8nn57lz0l484mbk"))

(define rust-iced-runtime-0.13.2
  (crate-source "iced_runtime" "0.13.2"
                "1db2xylrsrp9ihlglrka10yr47i92gcixvdhlf6dhd69c4n5p2rl"))

(define rust-iced-tiny-skia-0.13.0
  (crate-source "iced_tiny_skia" "0.13.0"
                "0l1ajr0xd5v8rvc8pmcxjpkypazixxv6y5vbnfq47k2g51ld69f6"))

(define rust-iced-wgpu-0.13.5
  (crate-source "iced_wgpu" "0.13.5"
                "0pr9k3j3wv1rjgw2nfywdi6schvg2wgivl61qsyd4w9n2f3qhw0m"))

(define rust-iced-widget-0.13.4
  (crate-source "iced_widget" "0.13.4"
                "079yk3wajvzjg0bl22h3hakqwrx6zrw44k5ycp54n3hbjldrwhl1"))

(define rust-iced-winit-0.13.0
  (crate-source "iced_winit" "0.13.0"
                "0a6s5wm9hparm8ikrw7d3z9i9fkjz5xr70lj817k7dllqphx8k7l"))

(define rust-icu-collections-1.5.0
  (crate-source "icu_collections" "1.5.0"
                "09j5kskirl59mvqc8kabhy7005yyy7dp88jw9f6f3gkf419a8byv"))

(define rust-icu-collections-2.0.0
  (crate-source "icu_collections" "2.0.0"
                "0izfgypv1hsxlz1h8fc2aak641iyvkak16aaz5b4aqg3s3sp4010"))

(define rust-icu-collections-2.1.1
  (crate-source "icu_collections" "2.1.1"
                "0hsblchsdl64q21qwrs4hvc2672jrf466zivbj1bwyv606bn8ssc"))

(define rust-icu-locale-core-2.0.0
  (crate-source "icu_locale_core" "2.0.0"
                "02phv7vwhyx6vmaqgwkh2p4kc2kciykv2px6g4h8glxfrh02gphc"))

(define rust-icu-locale-core-2.1.1
  (crate-source "icu_locale_core" "2.1.1"
                "1djvdc2f5ylmp1ymzv4gcnmq1s4hqfim9nxlcm173lsd01hpifpd"))

(define rust-icu-locid-1.5.0
  (crate-source "icu_locid" "1.5.0"
                "0dznvd1c5b02iilqm044q4hvar0sqibq1z46prqwjzwif61vpb0k"))

(define rust-icu-locid-transform-1.5.0
  (crate-source "icu_locid_transform" "1.5.0"
                "0kmmi1kmj9yph6mdgkc7v3wz6995v7ly3n80vbg0zr78bp1iml81"))

(define rust-icu-locid-transform-data-1.5.0
  (crate-source "icu_locid_transform_data" "1.5.0"
                "0vkgjixm0wzp2n3v5mw4j89ly05bg3lx96jpdggbwlpqi0rzzj7x"))

(define rust-icu-locid-transform-data-1.5.1
  (crate-source "icu_locid_transform_data" "1.5.1"
                "07gignya9gzynnyds88bmra4blq9jxzgrcss43vzk2q9h7byc5bm"))

(define rust-icu-normalizer-1.5.0
  (crate-source "icu_normalizer" "1.5.0"
                "0kx8qryp8ma8fw1vijbgbnf7zz9f2j4d14rw36fmjs7cl86kxkhr"))

(define rust-icu-normalizer-2.0.0
  (crate-source "icu_normalizer" "2.0.0"
                "0ybrnfnxx4sf09gsrxri8p48qifn54il6n3dq2xxgx4dw7l80s23"))

(define rust-icu-normalizer-2.1.1
  (crate-source "icu_normalizer" "2.1.1"
                "16dmn5596la2qm0r3vih0bzjfi0vx9a20yqjha6r1y3vnql8hv2z"))

(define rust-icu-normalizer-data-1.5.0
  (crate-source "icu_normalizer_data" "1.5.0"
                "05lmk0zf0q7nzjnj5kbmsigj3qgr0rwicnn5pqi9n7krmbvzpjpq"))

(define rust-icu-normalizer-data-1.5.1
  (crate-source "icu_normalizer_data" "1.5.1"
                "1dqcm86spcqcs4jnra81yqq3g5bpw6bvf5iz621spj5x52137s65"))

(define rust-icu-normalizer-data-2.0.0
  (crate-source "icu_normalizer_data" "2.0.0"
                "1lvjpzxndyhhjyzd1f6vi961gvzhj244nribfpdqxjdgjdl0s880"))

(define rust-icu-normalizer-data-2.1.1
  (crate-source "icu_normalizer_data" "2.1.1"
                "02jnzizg6q75m41l6c13xc7nkc5q8yr1b728dcgfhpzw076wrvbs"))

(define rust-icu-properties-1.5.1
  (crate-source "icu_properties" "1.5.1"
                "1xgf584rx10xc1p7zjr78k0n4zn3g23rrg6v2ln31ingcq3h5mlk"))

(define rust-icu-properties-2.0.1
  (crate-source "icu_properties" "2.0.1"
                "0az349pjg8f18lrjbdmxcpg676a7iz2ibc09d2wfz57b3sf62v01"))

(define rust-icu-properties-2.1.1
  (crate-source "icu_properties" "2.1.1"
                "16gvnnxr1xry6vn5275a1s0z0c8scp7gdkzqla6hqv3nawqwsgz9"))

(define rust-icu-properties-2.1.2
  (crate-source "icu_properties" "2.1.2"
                "1v3lbmhhi7i6jgw51ikjb1p50qh5rb67grlkdnkc63l7zq1gq2q2"))

(define rust-icu-properties-data-1.5.0
  (crate-source "icu_properties_data" "1.5.0"
                "0scms7pd5a7yxx9hfl167f5qdf44as6r3bd8myhlngnxqgxyza37"))

(define rust-icu-properties-data-1.5.1
  (crate-source "icu_properties_data" "1.5.1"
                "1qm5vf17nyiwb87s3g9x9fsj32gkv4a7q7d2sblawx9vfncqgyw5"))

(define rust-icu-properties-data-2.0.1
  (crate-source "icu_properties_data" "2.0.1"
                "0cnn3fkq6k88w7p86w7hsd1254s4sl783rpz4p6hlccq74a5k119"))

(define rust-icu-properties-data-2.1.1
  (crate-source "icu_properties_data" "2.1.1"
                "16a80p8j371jkl10x26rh9gw6d1gyl7limpc008my15v8wv5p102"))

(define rust-icu-properties-data-2.1.2
  (crate-source "icu_properties_data" "2.1.2"
                "1bvpkh939rgzrjfdb7hz47v4wijngk0snmcgrnpwc9fpz162jv31"))

(define rust-icu-provider-1.5.0
  (crate-source "icu_provider" "1.5.0"
                "1nb8vvgw8dv2inqklvk05fs0qxzkw8xrg2n9vgid6y7gm3423m3f"))

(define rust-icu-provider-2.0.0
  (crate-source "icu_provider" "2.0.0"
                "1bz5v02gxv1i06yhdhs2kbwxkw3ny9r2vvj9j288fhazgfi0vj03"))

(define rust-icu-provider-2.1.1
  (crate-source "icu_provider" "2.1.1"
                "0576b7dizgyhpfa74kacv86y4g1p7v5ffd6c56kf1q82rvq2r5l5"))

(define rust-icu-provider-macros-1.5.0
  (crate-source "icu_provider_macros" "1.5.0"
                "1mjs0w7fcm2lcqmbakhninzrjwqs485lkps4hz0cv3k36y9rxj0y"))

(define rust-id-arena-2.2.1
  (crate-source "id-arena" "2.2.1"
                "01ch8jhpgnih8sawqs44fqsqpc7bzwgy0xpi6j0f4j0i5mkvr8i5"))

(define rust-idea-0.5.1
  (crate-source "idea" "0.5.1"
                "0xv4hd9mgrwgzfl7cc5nlwyahm9yni5z9dwb3c1z5mqr8h05fm87"))

(define rust-ident-case-1.0.1
  (crate-source "ident_case" "1.0.1"
                "0fac21q6pwns8gh1hz3nbq15j8fi441ncl6w4vlnd1cmc55kiq5r"))

(define rust-idna-0.5.0
  (crate-source "idna" "0.5.0"
                "1xhjrcjqq0l5bpzvdgylvpkgk94panxgsirzhjnnqfdgc4a9nkb3"))

(define rust-idna-1.0.3
  (crate-source "idna" "1.0.3"
                "0zlajvm2k3wy0ay8plr07w22hxkkmrxkffa6ah57ac6nci984vv8"))

(define rust-idna-1.1.0
  (crate-source "idna" "1.1.0"
                "1pp4n7hppm480zcx411dsv9wfibai00wbpgnjj4qj0xa7kr7a21v"))

(define rust-idna-adapter-1.2.0
  (crate-source "idna_adapter" "1.2.0"
                "0wggnkiivaj5lw0g0384ql2d7zk4ppkn3b1ry4n0ncjpr7qivjns"))

(define rust-idna-adapter-1.2.1
  (crate-source "idna_adapter" "1.2.1"
                "0i0339pxig6mv786nkqcxnwqa87v4m94b2653f6k3aj0jmhfkjis"))

(define rust-ignore-0.4.23
  (crate-source "ignore" "0.4.23"
                "0jysggjfmlxbg60vhhiz4pb8jfb7cnq5swdsvxknbs7x18wgv2bd"))

(define rust-ignore-0.4.25
  (crate-source "ignore" "0.4.25"
                "0jlv2s4fxqj9fsz6y015j5vbz6i475hj80j9q3sy05d0cniq5myk"))

(define rust-image-0.24.9
  (crate-source "image" "0.24.9"
                "17gnr6ifnpzvhjf6dwbl9hki8x6bji5mwcqp0048x1jm5yfi742n"))

(define rust-image-0.25.6
  (crate-source "image" "0.25.6"
                "06i522bq4qlwylwnlmcn0sgqg72swwan544aldbhi0drwr66cdfv"))

(define rust-image-0.25.8
  (crate-source "image" "0.25.8"
                "1rwill018gn2kwzv332kfs72ns0kwwnfxwacbhvk9lk9cwzfp7sj"))

(define rust-image-0.25.9
  (crate-source "image" "0.25.9"
                "06lwa4ag3zcmjzivl356q0qhgxxqpkp7qwda7x0mjrkq21n6ql76"))

(define rust-image-webp-0.2.3
  (crate-source "image-webp" "0.2.3"
                "02vg5rfmkvslypd0xij6ynzaah3m32hfylicwr14n2rhlpkhz5zn"))

(define rust-image-webp-0.2.4
  (crate-source "image-webp" "0.2.4"
                "1hz814csyi9283vinzlkix6qpnd6hs3fkw7xl6z2zgm4w7rrypjj"))

(define rust-imagesize-0.12.0
  (crate-source "imagesize" "0.12.0"
                "114jvqiyv13il1qghv2xm0xqrcjm68fh282hdlzdds6qfgsp7782"))

(define rust-imagesize-0.13.0
  (crate-source "imagesize" "0.13.0"
                "11f26ac9zvbr7sjnsv2z9jd3ryaz40pg8xch4ij1q1rg5zbjgkgd"))

(define rust-imap-client-0.2.3
  (crate-source "imap-client" "0.2.3"
                "0812nlk2sadi5k9c74n5mksb73plxcr7rjk4gnd8xi460rabv0g7"))

(define rust-imap-codec-2.0.0-alpha.5
  (crate-source "imap-codec" "2.0.0-alpha.5"
                "0mw5n97wskdf861032x63sn694pckwkprw74i3i8zyyivl53317m"))

(define rust-imap-next-0.3.1
  (crate-source "imap-next" "0.3.1"
                "0hwvdga7rg32wnfqznshhv80wmkghzhgk7dzxpcyina2wwh5bn2n"))

(define rust-imap-types-2.0.0-alpha.4
  (crate-source "imap-types" "2.0.0-alpha.4"
                "0lzgr4lihafycakpiadlch1y6w0p67bmllrdrjd68aln24gxh0fn"))

(define rust-imara-diff-0.1.8
  (crate-source "imara-diff" "0.1.8"
                "1lmk5dpha2fhahrnsrgavxn1qz6ydp1w8jz8fpvlb28p89ylplqp"))

(define rust-imgref-1.12.0
  (crate-source "imgref" "1.12.0"
                "1j3iwdal9mdkmyrsms3lz4n1bxxxjxss2jvbmh662fns63fcxig7"))

(define rust-impl-more-0.1.9
  (crate-source "impl-more" "0.1.9"
                "1llwkdr56n340md0nh0h9m8gkbdymax45fdc92hwg1h0zyhak9g8"))

(define rust-include-dir-0.7.4
  (crate-source "include_dir" "0.7.4"
                "1pfh3g45z88kwq93skng0n6g3r7zkhq9ldqs9y8rvr7i11s12gcj"))

(define rust-include-dir-macros-0.7.4
  (crate-source "include_dir_macros" "0.7.4"
                "0x8smnf6knd86g69p19z5lpfsaqp8w0nx14kdpkz1m8bxnkqbavw"))

(define rust-indenter-0.3.3
  (crate-source "indenter" "0.3.3"
                "10y6i6y4ls7xsfsc1r3p5j2hhbxhaqnk5zzk8aj52b14v05ba8yf"))

(define rust-indexmap-2.10.0
  (crate-source "indexmap" "2.10.0"
                "0qd6g26gxzl6dbf132w48fa8rr95glly3jhbk90i29726d9xhk7y"))

(define rust-indexmap-2.11.4
  (crate-source "indexmap" "2.11.4"
                "1rc8bgcjzfcskz1zipjjm7s3m1jskzhnhr9jxmsafhdk1xv863sb"))

(define rust-indexmap-2.12.0
  (crate-source "indexmap" "2.12.0"
                "17xs7cqf9nzv8iw8yzpvpjh43lcf9492i8a3xfia2ad9lp9ah5v7"))

(define rust-indexmap-2.12.1
  (crate-source "indexmap" "2.12.1"
                "1wmcfk7g7d9wz1dninlijx70kfkzz6d5r36nyi2hdjjvaqmvpm0a"))

(define rust-indexmap-2.13.0
  (crate-source "indexmap" "2.13.0"
                "05qh5c4h2hrnyypphxpwflk45syqbzvqsvvyxg43mp576w2ff53p"))

(define rust-indexmap-2.2.2
  (crate-source "indexmap" "2.2.2"
                "087mafd9f98rp1xk2jc1rsp5yyqz63yi30cy8yx6c8s14bj2ljw2"))

(define rust-indexmap-2.2.6
  (crate-source "indexmap" "2.2.6"
                "09hgwi2ig0wyj5rjziia76zmhgfj95k0jb4ic3iiawm4vlavg3qn"))

(define rust-indexmap-2.3.0
  (crate-source "indexmap" "2.3.0"
                "182yvyck0zk3swa9bs8l9a8lmis3q7zy338wj6rx2bd81giw4gyy"))

(define rust-indexmap-2.7.0
  (crate-source "indexmap" "2.7.0"
                "07s7jmdymvd0rm4yswp0j3napx57hkjm9gs9n55lvs2g78vj5y32"))

(define rust-indexmap-2.7.1
  (crate-source "indexmap" "2.7.1"
                "0lmnm1zbr5gq3wic3d8a76gpvampridzwckfl97ckd5m08mrk74c"))

(define rust-indexmap-2.8.0
  (crate-source "indexmap" "2.8.0"
                "0n3hkpzch6q3wgzh8g8hiyac6kk3vgd8nfsxy8mi80jvw47xam1r"))

(define rust-indexmap-2.9.0
  (crate-source "indexmap" "2.9.0"
                "07m15a571yywmvqyb7ms717q9n42b46badbpsmx215jrg7dhv9yf"))

(define rust-indoc-2.0.6
  (crate-source "indoc" "2.0.6"
                "1gbn2pkx5sgbd9lp05d2bkqpbfgazi0z3nvharh5ajah11d29izl"))

(define rust-indoc-2.0.7
  (crate-source "indoc" "2.0.7"
                "01np60qdq6lvgh8ww2caajn9j4dibx9n58rvzf7cya1jz69mrkvr"))

(define rust-inflector-0.11.4
  (crate-source "Inflector" "0.11.4"
                "1lqmcni21ifzyq41fhz6k1j2b23cmsx469s4g4sf01l78miqqhzy"))

(define rust-inotify-0.11.0
  (crate-source "inotify" "0.11.0"
                "1wq8m657rl085cg59p38sc5y62xy9yhhpvxbkd7n1awi4zzwqzgk"))

(define rust-inotify-0.9.6
  (crate-source "inotify" "0.9.6"
                "1zxb04c4qccp8wnr3v04l503qpxzxzzzph61amlqbsslq4z9s1pq"))

(define rust-inotify-sys-0.1.5
  ;; TODO: Check bundled sources.
  (crate-source "inotify-sys" "0.1.5"
                "1syhjgvkram88my04kv03s0zwa66mdwa5v7ddja3pzwvx2sh4p70"))

(define rust-inout-0.1.3
  (crate-source "inout" "0.1.3"
                "1xf9gf09nc7y1a261xlfqsf66yn6mb81ahlzzyyd1934sr9hbhd0"))

(define rust-inquire-0.7.5
  (crate-source "inquire" "0.7.5"
                "0an1bjs0yklk990d7ni65znmirs99p01wld221affw5g64qgkp8g"))

(define rust-insta-1.42.2
  (crate-source "insta" "1.42.2"
                "111hrdc3bxwp146kz2ffwdq0qypdjk8a2yzwr8mivlb7maxrl9ah"))

(define rust-insta-1.43.2
  (crate-source "insta" "1.43.2"
                "181m173v4f7s2f3j0lq462s0r6yg60y7fgxmnm1hy06yxd3vdza6"))

(define rust-instability-0.3.10
  (crate-source "instability" "0.3.10"
                "170dsap2il7fpx85dylb4q979czrbj77ay6v77vpvvpgdqcv0y37"))

(define rust-instability-0.3.11
  (crate-source "instability" "0.3.11"
                "07f1apjp00nzkwmzfzlfm6p4klddf0g2scgdhqnds66dqq2p4yrm"))

(define rust-instability-0.3.6
  (crate-source "instability" "0.3.6"
                "0gsvkqy6ximw97f1glz13nic81yif4bh86r21s60r34h8jj16j49"))

(define rust-instability-0.3.9
  (crate-source "instability" "0.3.9"
                "16pbfp50y1f6qqifk77zymz7r20f96v2gjc84vb8frwk1f080pa3"))

(define rust-instant-0.1.12
  (crate-source "instant" "0.1.12"
                "0b2bx5qdlwayriidhrag8vhy10kdfimfhmb3jnjmsz2h9j1bwnvs"))

(define rust-instant-0.1.13
  (crate-source "instant" "0.1.13"
                "08h27kzvb5jw74mh0ajv0nv9ggwvgqm8ynjsn2sa9jsks4cjh970"))

(define rust-interim-0.2.1
  (crate-source "interim" "0.2.1"
                "0njv1x7v55mr90ib278y8il8jm6hhmzghnr2sdiqcijzm2cr1km9"))

(define rust-interpolate-name-0.2.4
  (crate-source "interpolate_name" "0.2.4"
                "0q7s5mrfkx4p56dl8q9zq71y1ysdj4shh6f28qf9gly35l21jj63"))

(define rust-io-kit-sys-0.4.1
  ;; TODO: Check bundled sources.
  (crate-source "io-kit-sys" "0.4.1"
                "0ysy5k3wf54yangy25hkj10xx332cj2hb937xasg6riziv7yczk1"))

(define rust-io-lifetimes-1.0.10
  (crate-source "io-lifetimes" "1.0.10"
                "08625nsz0lgbd7c9lly6b6l45viqpsnj9jbsixd9mrz7596wfrlw"))

(define rust-io-uring-0.7.10
  (crate-source "io-uring" "0.7.10"
                "0yvjyygwdcqjcgw8zp254hvjbm7as1c075dl50spdshas3aa4vq4"))

(define rust-iovec-0.1.4
  (crate-source "iovec" "0.1.4"
                "0ph73qygwx8i0mblrf110cj59l00gkmsgrpzz1rm85syz5pymcxj"))

(define rust-ipconfig-0.3.2
  (crate-source "ipconfig" "0.3.2"
                "0zwr0x3jnqmjdqqbzhb0nid011qyhcyfdfqv32cdw85pjqpvk3dm"))

(define rust-ipnet-2.10.1
  (crate-source "ipnet" "2.10.1"
                "025p9wm94q1w2l13hbbr4cbmfygly3a2ag8g5s618l2jhq4l3hnx"))

(define rust-ipnet-2.11.0
  (crate-source "ipnet" "2.11.0"
                "0c5i9sfi2asai28m8xp48k5gvwkqrg5ffpi767py6mzsrswv17s6"))

(define rust-ipnet-2.7.2
  (crate-source "ipnet" "2.7.2"
                "0zxmnidy5qha1i384fzjfxcsi0qvkbcp730h26q4z3dg54hyxdhj"))

(define rust-ipnet-2.9.0
  (crate-source "ipnet" "2.9.0"
                "1hzrcysgwf0knf83ahb3535hrkw63mil88iqc6kjaryfblrqylcg"))

(define rust-ipnetwork-0.20.0
  (crate-source "ipnetwork" "0.20.0"
                "03hhmxyimz0800z44wl3z1ak8iw91xcnk7sgx5p5jinmx50naimz"))

(define rust-ipnetwork-0.21.1
  (crate-source "ipnetwork" "0.21.1"
                "0qqpyjfma524v734nw4xri38xwj524z8q3k29bji6kfmmyyhldyg"))

(define rust-iri-string-0.7.10
  (crate-source "iri-string" "0.7.10"
                "06kk3a5jz576p7vrpf7zz9jv3lrgcyp7pczcblcxdnryg3q3h4y9"))

(define rust-iri-string-0.7.8
  (crate-source "iri-string" "0.7.8"
                "1cl0wfq97wq4s1p4dl0ix5cfgsc5fn7l22ljgw9ab9x1qglypifv"))

(define rust-is-docker-0.2.0
  (crate-source "is-docker" "0.2.0"
                "1cyibrv6817cqcpf391m327ss40xlbik8wxcv5h9pj9byhksx2wj"))

(define rust-is-executable-1.0.4
  (crate-source "is_executable" "1.0.4"
                "1qlafm7f0zq0kzvbd4fhcfci4g9gxp6g3yqxjqsjj1zrssxbb8fl"))

(define rust-is-executable-1.0.5
  (crate-source "is_executable" "1.0.5"
                "1i78ss45h94nwabbn6ki64a91djlli8zdwwbh56jj9kvhssbiaxs"))

(define rust-is-terminal-0.4.13
  (crate-source "is-terminal" "0.4.13"
                "0jwgjjz33kkmnwai3nsdk1pz9vb6gkqvw1d1vq7bs3q48kinh7r6"))

(define rust-is-terminal-polyfill-1.70.1
  (crate-source "is_terminal_polyfill" "1.70.1"
                "1kwfgglh91z33kl0w5i338mfpa3zs0hidq5j4ny4rmjwrikchhvr"))

(define rust-is-terminal-polyfill-1.70.2
  (crate-source "is_terminal_polyfill" "1.70.2"
                "15anlc47sbz0jfs9q8fhwf0h3vs2w4imc030shdnq54sny5i7jx6"))

(define rust-is-wsl-0.4.0
  (crate-source "is-wsl" "0.4.0"
                "19bs5pq221d4bknnwiqqkqrnsx2in0fsk8fylxm1747iim4hjdhp"))

(define rust-itertools-0.10.5
  (crate-source "itertools" "0.10.5"
                "0ww45h7nxx5kj6z2y6chlskxd1igvs4j507anr6dzg99x1h25zdh"))

(define rust-itertools-0.12.1
  (crate-source "itertools" "0.12.1"
                "0s95jbb3ndj1lvfxyq5wanc0fm0r6hg6q4ngb92qlfdxvci10ads"))

(define rust-itertools-0.13.0
  (crate-source "itertools" "0.13.0"
                "11hiy3qzl643zcigknclh446qb9zlg4dpdzfkjaa9q9fqpgyfgj1"))

(define rust-itertools-0.14.0
  (crate-source "itertools" "0.14.0"
                "118j6l1vs2mx65dqhwyssbrxpawa90886m3mzafdvyip41w2q69b"))

(define rust-itertools-0.5.10
  (crate-source "itertools" "0.5.10"
                "1z4lyrakgynvhylya72qb3vizmxmd62whjmg4r8k01d4inbxccs8"))

(define rust-itoa-1.0.10
  (crate-source "itoa" "1.0.10"
                "0k7xjfki7mnv6yzjrbnbnjllg86acmbnk4izz2jmm1hx2wd6v95i"))

(define rust-itoa-1.0.11
  (crate-source "itoa" "1.0.11"
                "0nv9cqjwzr3q58qz84dcz63ggc54yhf1yqar1m858m1kfd4g3wa9"))

(define rust-itoa-1.0.14
  (crate-source "itoa" "1.0.14"
                "0x26kr9m062mafaxgcf2p6h2x7cmixm0zw95aipzn2hr3d5jlnnp"))

(define rust-itoa-1.0.15
  (crate-source "itoa" "1.0.15"
                "0b4fj9kz54dr3wam0vprjwgygvycyw8r0qwg7vp19ly8b2w16psa"))

(define rust-itoa-1.0.17
  (crate-source "itoa" "1.0.17"
                "1lh93xydrdn1g9x547bd05g0d3hra7pd1k4jfd2z1pl1h5hwdv4j"))

(define rust-itoa-1.0.6
  (crate-source "itoa" "1.0.6"
                "19jc2sa3wvdc29zhgbwf3bayikq4rq18n20dbyg9ahd4hbsxjfj5"))

(define rust-itoa-1.0.9
  (crate-source "itoa" "1.0.9"
                "0f6cpb4yqzhkrhhg6kqsw3wnmmhdnnffi6r2xzy248gzi2v0l5dg"))

(define rust-jiff-0.2.15
  (crate-source "jiff" "0.2.15"
                "0jby6kbs2ra33ji0rx4swcp66jzmcvgszc5v4izwfsgbn6w967xy"))

(define rust-jiff-0.2.5
  (crate-source "jiff" "0.2.5"
                "0q3jpq3scznmviiajldyf5xby38zgyvkxbrmgb9hf78r6416f0n1"))

(define rust-jiff-static-0.2.15
  (crate-source "jiff-static" "0.2.15"
                "1d4l4pvlhz3w487gyhnzvagpbparspv4c8f35qk6g5w9zx8k8d03"))

(define rust-jiff-static-0.2.5
  (crate-source "jiff-static" "0.2.5"
                "0k1v30mhbgh4zj2r9d7lfqlh5b20b5573cx0a4gip7rlkldf7pac"))

(define rust-jiff-tzdb-0.1.4
  (crate-source "jiff-tzdb" "0.1.4"
                "09350bna4vxdn2fv7gd08ay41llkflmfyvpx5d6l088axc2kfa61"))

(define rust-jiff-tzdb-platform-0.1.3
  (crate-source "jiff-tzdb-platform" "0.1.3"
                "1s1ja692wyhbv7f60mc0x90h7kn1pv65xkqi2y4imarbmilmlnl7"))

(define rust-jj-lib-0.28.2
  (crate-source "jj-lib" "0.28.2"
                "1m57p2krpldczz6hm4vi98siqmfv8vfm32lm50mm8lcx4yfrrpjy"))

(define rust-jj-lib-0.35.0
  (crate-source "jj-lib" "0.35.0"
                "1mwkrlfdxs66p1v3mkij5wds26apm46pcc8ab4b77g2g3rp90nbk"))

(define rust-jj-lib-proc-macros-0.28.2
  (crate-source "jj-lib-proc-macros" "0.28.2"
                "0ix2pbhwrk07gplwn1d0bx8zb01g2lvi8dj65fz9zlmsiidg3zf2"))

(define rust-jj-lib-proc-macros-0.35.0
  (crate-source "jj-lib-proc-macros" "0.35.0"
                "0701skbg15ddm36dm5881hkclhjmkpz1fg516lh8nqgakfh34q9j"))

(define rust-jmap-tools-0.1.2
  (crate-source "jmap-tools" "0.1.2"
                "1cdl8ivax1sryxmqqphash4aw4k0zrzvc5x391m6xh30z0gjw8li"))

(define rust-jni-0.19.0
  (crate-source "jni" "0.19.0"
                "1v0pn0i1wb8zp4wns4l8hz9689hqsplv7iba7hylaznvwg11ipy6"))

(define rust-jni-0.21.1
  (crate-source "jni" "0.21.1"
                "15wczfkr2r45slsljby12ymf2hij8wi5b104ghck9byjnwmsm1qs"))

(define rust-jni-sys-0.3.0
  ;; TODO: Check bundled sources.
  (crate-source "jni-sys" "0.3.0"
                "0c01zb9ygvwg9wdx2fii2d39myzprnpqqhy7yizxvjqp5p04pbwf"))

(define rust-jobserver-0.1.32
  (crate-source "jobserver" "0.1.32"
                "1l2k50qmj84x9mn39ivjz76alqmx72jhm12rw33zx9xnpv5xpla8"))

(define rust-jobserver-0.1.33
  (crate-source "jobserver" "0.1.33"
                "12jkn3cxvfs7jsb6knmh9y2b41lwmrk3vdqywkmssx61jzq65wiq"))

(define rust-jobserver-0.1.34
  (crate-source "jobserver" "0.1.34"
                "0cwx0fllqzdycqn4d6nb277qx5qwnmjdxdl0lxkkwssx77j3vyws"))

(define rust-jpeg-decoder-0.3.1
  (crate-source "jpeg-decoder" "0.3.1"
                "1c1k53svpdyfhibkmm0ir5w0v3qmcmca8xr8vnnmizwf6pdagm7m"))

(define rust-jpeg-decoder-0.3.2
  (crate-source "jpeg-decoder" "0.3.2"
                "01qs04rnackrc5xhw7w9difn2h3pqsdbhgdz7nqn9gklicfhz080"))

(define rust-jpeg-encoder-0.5.1
  (crate-source "jpeg-encoder" "0.5.1"
                "01j9wslaqs0ragv3i5xkkjz2l8lbarspbv4h4scz3ngz4zzazwrc"))

(define rust-js-sys-0.3.61
  ;; TODO: Check bundled sources.
  (crate-source "js-sys" "0.3.61"
                "0c075apyc5fxp2sbgr87qcvq53pcjxmp05l47lzlhpn5a0hxwpa4"))

(define rust-js-sys-0.3.69
  ;; TODO: Check bundled sources.
  (crate-source "js-sys" "0.3.69"
                "0v99rz97asnzapb0jsc3jjhvxpfxr7h7qd97yqyrf9i7viimbh99"))

(define rust-js-sys-0.3.70
  ;; TODO: Check bundled sources.
  (crate-source "js-sys" "0.3.70"
                "0yp3rz7vrn9mmqdpkds426r1p9vs6i8mkxx8ryqdfadr0s2q0s0q"))

(define rust-js-sys-0.3.72
  ;; TODO: Check bundled sources.
  (crate-source "js-sys" "0.3.72"
                "1a8r61hbgw5kmscgj3g5pzg2ywlnswvljy0l592v0xdxlayz323a"))

(define rust-js-sys-0.3.76
  ;; TODO: Check bundled sources.
  (crate-source "js-sys" "0.3.76"
                "1dz7v777h2j38wkf8k5iwkfxskn6nff2cdv2jsslyxkpn2svc5v7"))

(define rust-js-sys-0.3.77
  ;; TODO: Check bundled sources.
  (crate-source "js-sys" "0.3.77"
                "13x2qcky5l22z4xgivi59xhjjx4kxir1zg7gcj0f1ijzd4yg7yhw"))

(define rust-js-sys-0.3.81
  ;; TODO: Check bundled sources.
  (crate-source "js-sys" "0.3.81"
                "01ckbf16iwh7qj92fax9zh8vf2y9sk60cli6999cn7a1jxx96j7c"))

(define rust-js-sys-0.3.82
  ;; TODO: Check bundled sources.
  (crate-source "js-sys" "0.3.82"
                "0rcfkz7r28r01a8m6n2q13aglm44q5g6gzsw5nmjmninrk4fw4dh"))

(define rust-js-sys-0.3.83
  ;; TODO: Check bundled sources.
  (crate-source "js-sys" "0.3.83"
                "1n71vpxrzclly0530lwkcsx6mg73lipam2ak3rr1ypzmqw4kfjj6"))

(define rust-js-sys-0.3.85
  ;; TODO: Check bundled sources.
  (crate-source "js-sys" "0.3.85"
                "1csmb42fxjmzjdgc790bgw77sf1cb9ydm5rdsnh5qj4miszjx54c"))

(define rust-jsonschema-0.33.0
  (crate-source "jsonschema" "0.33.0"
                "1dcnrddf1j6gfkgcbxjxwhlyi16wp8pn7x3mbca0mxn5kf2n4rnl"))

(define rust-jsonschema-0.37.4
  (crate-source "jsonschema" "0.37.4"
                "0zjafvj6ijxm6p7iiw8m0mcr9nl9hf72slqv1q1mhvf5nnrgzjbk"))

(define rust-jxl-bitstream-0.2.3
  (crate-source "jxl-bitstream" "0.2.3"
                "08k6dy6v5qwlaplc02vln2pzfvw8jbjndpllysrjjqng31fj1hvn"))

(define rust-jxl-coding-0.2.3
  (crate-source "jxl-coding" "0.2.3"
                "1d6h5ms7ffclrag48892zq0i9byjvsl3zg0zvpifj71cfq660xbh"))

(define rust-jxl-color-0.3.2
  (crate-source "jxl-color" "0.3.2"
                "0wvfiyqf0pmm189g6d83g3iz8mqslla7a63cvfqjc7nhnqrvr14h"))

(define rust-jxl-frame-0.5.0
  (crate-source "jxl-frame" "0.5.0"
                "1qa5bvkxsrzqlhb5z9n5r7ymjpda83wqdpi3l5lncx730k8vsqwd"))

(define rust-jxl-grid-0.1.1
  (crate-source "jxl-grid" "0.1.1"
                "1s2547v112wvljkxamqz44dgpg9h0pa9rbhqz31bpcvbxlhhp028"))

(define rust-jxl-image-0.5.0
  (crate-source "jxl-image" "0.5.0"
                "0ndyc28vppfrdg9dg8sy8l65mvyd007073acc1k9x76c9bvzg1pg"))

(define rust-jxl-modular-0.3.0
  (crate-source "jxl-modular" "0.3.0"
                "0j377r653p32zxlyfymndz2qcrzwfb3r66g85dcnh99nvdannkjh"))

(define rust-jxl-oxide-0.4.0
  (crate-source "jxl-oxide" "0.4.0"
                "0r30kcp0hfs4xab4bwrp8haxp08m757mijd09jf9f8yqb7jbgqsp"))

(define rust-jxl-render-0.4.0
  (crate-source "jxl-render" "0.4.0"
                "0zkhr03nckj8d6gg6lgwkamifx5s8njqpk5h1j0dqvc9qk3d2mvi"))

(define rust-jxl-vardct-0.3.0
  (crate-source "jxl-vardct" "0.3.0"
                "0gh52l6na3yn9kwajfdrr44i6zf6m8w1r81v2kv552n4m2djsjpb"))

(define rust-kamadak-exif-0.5.5
  (crate-source "kamadak-exif" "0.5.5"
                "0xw0lpmra8j1y98c0agwrmjajpkh91mnl89hzaxbdrdp186wfkzg"))

(define rust-kanal-0.1.1
  (crate-source "kanal" "0.1.1"
                "0x7m3wnl85n4qia13pfpd4r9nv9dal9zmhlnnfc7frndy2nm6fcy"))

(define rust-kasuari-0.4.11
  (crate-source "kasuari" "0.4.11"
                "0nqa4gkq9jgznnqs8yxzv200lysiny4m152zgn68abk6a08hrscg"))

(define rust-keccak-0.1.5
  (crate-source "keccak" "0.1.5"
                "0m06swsyd58hvb1z17q6picdwywprf1yf1s6l491zi8r26dazhpc"))

(define rust-keyring-3.6.1
  (crate-source "keyring" "3.6.1"
                "0cq8y72lwpl2n4n2hxz8lxwsz5in0q8fkmsnswvlnkjd8qwyi3rg"))

(define rust-keyring-lib-1.0.2
  (crate-source "keyring-lib" "1.0.2"
                "062jjqr9l7xs8cxnb6cvns32mkb4cyvqcyq4qr8kvwv5qic1b4jn"))

(define rust-khronos-api-3.1.0
  (crate-source "khronos_api" "3.1.0"
                "1p0xj5mlbagqyvvnv8wmv3cr7l9y1m153888pxqwg3vk3mg5inz2"))

(define rust-khronos-egl-6.0.0
  (crate-source "khronos-egl" "6.0.0"
                "0xnzdx0n1bil06xmh8i1x6dbxvk7kd2m70bbm6nw1qzc43r1vbka"))

(define rust-kqueue-1.0.7
  (crate-source "kqueue" "1.0.7"
                "165dvjbf7s0nv6q8r9zkzi1v0g81c65a81wqm9bi5xavl45wd3rc"))

(define rust-kqueue-1.0.8
  (crate-source "kqueue" "1.0.8"
                "033x2knkbv8d3jy6i9r32jcgsq6zm3g97zh5la43amkv3g5g2ivl"))

(define rust-kqueue-1.1.1
  (crate-source "kqueue" "1.1.1"
                "0sjrsnza8zxr1zfpv6sa0zapd54kx9wlijrz9apqvs6wsw303hza"))

(define rust-kqueue-sys-1.0.3
  ;; TODO: Check bundled sources.
  (crate-source "kqueue-sys" "1.0.3"
                "11z5labbms9vn9m6csi5383dmrlmdgsxq13ls9fwa6zhi5a5hrw3"))

(define rust-kqueue-sys-1.0.4
  ;; TODO: Check bundled sources.
  (crate-source "kqueue-sys" "1.0.4"
                "12w3wi90y4kwis4k9g6fp0kqjdmc6l00j16g8mgbhac7vbzjb5pd"))

(define rust-kstring-2.0.2
  (crate-source "kstring" "2.0.2"
                "1lfvqlqkg2x23nglznb7ah6fk3vv3y5i759h5l2151ami98gk2sm"))

(define rust-kurbo-0.10.4
  (crate-source "kurbo" "0.10.4"
                "0h1lqqdxv2xd5lyvpfsw01hyjpzkms03sdndwxkpvs93v7mx860n"))

(define rust-kurbo-0.11.1
  (crate-source "kurbo" "0.11.1"
                "13sij8pmjp8rn8gwzx5fzx623m55s4fv9rmxgs9dv9qhqqn4n8w9"))

(define rust-kurbo-0.11.3
  (crate-source "kurbo" "0.11.3"
                "0qiwq4l83fy9v5d1piywvh44yg9ha3rl04d2kdcqlvvm8jp2c866"))

(define rust-lab-0.11.0
  (crate-source "lab" "0.11.0"
                "13ymsn5cwl5i9pmp5mfmbap7q688dcp9a17q82crkvb784yifdmz"))

(define rust-language-tags-0.3.2
  (crate-source "language-tags" "0.3.2"
                "124k6w9nx33q4xs8rpa9f7klshrsa0x4f7qngdwq890lpdj5jd6l"))

(define rust-lazy-regex-3.4.1
  (crate-source "lazy-regex" "3.4.1"
                "09kiz9pmpfj8ddfr6kfrxg5kqinkk3llvsm7iywkcav8jc5k3iv0"))

(define rust-lazy-regex-3.4.2
  (crate-source "lazy-regex" "3.4.2"
                "0abvvbncsj2dzlhhxid8sknq37ik0am4affbpih9xqfygphrh60r"))

(define rust-lazy-regex-proc-macros-3.4.1
  (crate-source "lazy-regex-proc_macros" "3.4.1"
                "1886sbdzfsqmgnyzsw1hmjdy58xks44j23sy1aqpxqc1xysiv82b"))

(define rust-lazy-regex-proc-macros-3.4.2
  (crate-source "lazy-regex-proc_macros" "3.4.2"
                "011yb7m86fj8j9rk851hc5y9q6m7vs02q4hjgr8aklc3vaqchpf3"))

(define rust-lazy-static-1.4.0
  (crate-source "lazy_static" "1.4.0"
                "0in6ikhw8mgl33wjv6q6xfrb5b9jr16q8ygjy803fay4zcisvaz2"))

(define rust-lazy-static-1.5.0
  (crate-source "lazy_static" "1.5.0"
                "1zk6dqqni0193xg6iijh7i3i44sryglwgvx20spdvwk3r6sbrlmv"))

(define rust-lazycell-1.3.0
  (crate-source "lazycell" "1.3.0"
                "0m8gw7dn30i0zjjpjdyf6pc16c34nl71lpv461mix50x3p70h3c3"))

(define rust-lddtree-0.4.0
  (crate-source "lddtree" "0.4.0"
                "17r3pnrfsrqxlcb1i8gxlkgaklnhhj054f9pfr0ymcin8001zmww"))

(define rust-leb128fmt-0.1.0
  (crate-source "leb128fmt" "0.1.0"
                "1chxm1484a0bly6anh6bd7a99sn355ymlagnwj3yajafnpldkv89"))

(define rust-lebe-0.5.3
  (crate-source "lebe" "0.5.3"
                "1f459clndzzm35nyd15vj5dlasyagfasp7hcgl6lh2b658rs6ybs"))

(define rust-lexopt-0.3.0
  (crate-source "lexopt" "0.3.0"
                "00dlvik2ygw8z101vf3bfndcvxhp92v25sbzz6bdiwvxgxhlpzxs"))

(define rust-lexopt-0.3.1
  (crate-source "lexopt" "0.3.1"
                "19zklfskhlvrfbl55h9g2a1v61k2jxcj4d1fqkk6nbxyzjhy584z"))

(define rust-lfs-core-0.14.1
  (crate-source "lfs-core" "0.14.1"
                "0sga4zdsal26s3fpdizgpakgzd80afbcfz07wsr2bczmqvgb4pdn"))

(define rust-lfs-core-0.17.0
  (crate-source "lfs-core" "0.17.0"
                "1l3gzzcvk5vjc0c70y7v812nkj21vswnv7kap5kb3s74rl2m2x60"))

(define rust-libadwaita-0.5.3
  (crate-source "libadwaita" "0.5.3"
                "174pzn9dwsk8ikvrhx13vkh0zrpvb3rhg9yd2q5d2zjh0q6fgrrg"))

(define rust-libadwaita-sys-0.5.3
  ;; TODO: Check bundled sources.
  (crate-source "libadwaita-sys" "0.5.3"
                "16n6xsy6jhbj0jbpz8yvql6c9b89a99v9vhdz5s37mg1inisl42y"))

(define rust-libc-0.2.153
  (crate-source "libc" "0.2.153"
                "1gg7m1ils5dms5miq9fyllrcp0jxnbpgkx71chd2i0lafa8qy6cw"))

(define rust-libc-0.2.155
  (crate-source "libc" "0.2.155"
                "0z44c53z54znna8n322k5iwg80arxxpdzjj5260pxxzc9a58icwp"))

(define rust-libc-0.2.161
  (crate-source "libc" "0.2.161"
                "1lc5s3zd0491x9zxrv2kvclai1my1spz950pkkyry4vwh318k54f"))

(define rust-libc-0.2.169
  (crate-source "libc" "0.2.169"
                "02m253hs8gw0m1n8iyrsc4n15yzbqwhddi7w1l0ds7i92kdsiaxm"))

(define rust-libc-0.2.170
  (crate-source "libc" "0.2.170"
                "0a38q3avb6r6azxb7yfbjly5sbr8926z6c4sryyp33rgrf03cnw7"))

(define rust-libc-0.2.171
  (crate-source "libc" "0.2.171"
                "1mipla3dy3l59pfa9xy4iw2vdgn8n30dzf4vdnasjflxdqhkg6f1"))

(define rust-libc-0.2.174
  (crate-source "libc" "0.2.174"
                "0xl7pqvw7g2874dy3kjady2fjr4rhj5lxsnxkkhr5689jcr6jw8i"))

(define rust-libc-0.2.175
  (crate-source "libc" "0.2.175"
                "0hw5sb3gjr0ivah7s3fmavlpvspjpd4mr009abmam2sr7r4sx0ka"))

(define rust-libc-0.2.176
  (crate-source "libc" "0.2.176"
                "0x7ivn80h7nz2l46vra7bxx36s6r8d0lkax14dx97skjsss2kyaq"))

(define rust-libc-0.2.177
  (crate-source "libc" "0.2.177"
                "0xjrn69cywaii1iq2lib201bhlvan7czmrm604h5qcm28yps4x18"))

(define rust-libc-0.2.178
  (crate-source "libc" "0.2.178"
                "1490yks6mria93i3xdva1gm05cjz824g14mbv0ph32lxma6kvj9p"))

(define rust-libc-0.2.180
  (crate-source "libc" "0.2.180"
                "1z2n7hl10fnk1xnv19ahhqxwnb4qi9aclnl6gigim2aaahw5mhxw"))

(define rust-libcorn-0.10.0
  (crate-source "libcorn" "0.10.0"
                "1qaaspxjjh28k9cbg6ad7gb99j95x3qdzan3rgldpi0gchxs1sny"))

(define rust-libcorn-0.9.2
  (crate-source "libcorn" "0.9.2"
                "07k7nwi7l0fix34jybrqbpvlghqk36j1rnvjbh4yn5f210bs9ba8"))

(define rust-libdav-0.10.0
  (crate-source "libdav" "0.10.0"
                "1zx0nbdwj0ang3la3pmlnv7g53q5xmh3h7mq0jq4p9qiyi0dcciz"))

(define rust-libdav-0.9.1
  (crate-source "libdav" "0.9.1"
                "1646mcnalav3jiprn3xyslyncmcvn34jzw5qn0h4k1x0bppczqhm"))

(define rust-libdbus-sys-0.2.5
  ;; TODO: Check bundled sources.
  (crate-source "libdbus-sys" "0.2.5"
                "0wjw93q6ckrn8qdrxzdi02f0ma9g7nnlpgkrkcll1mjhnw95a206"))

(define rust-libfuzzer-sys-0.4.10
  ;; TODO: Check bundled sources.
  (crate-source "libfuzzer-sys" "0.4.10"
                "0124z86582vyzl8gbadqscjgf9i94jcpa9mxcpsyxjvh3w71jdsh"))

(define rust-libgit2-sys-0.17.0+1.8.1
  ;; TODO: Check bundled sources.
  (crate-source "libgit2-sys" "0.17.0+1.8.1"
                "093jxfl2i9vxdlgf7vk9d040sjwy0nq4fid640y7qix6m0k26iqh"))

(define rust-libgit2-sys-0.18.1+1.9.0
  ;; TODO: Check bundled sources.
  (crate-source "libgit2-sys" "0.18.1+1.9.0"
                "03i98nb84aa99bn7sxja11pllq6fghsaw4d3qwjxikgzhh7v5p71"))

(define rust-libgit2-sys-0.18.2+1.9.1
  ;; TODO: Check bundled sources.
  (crate-source "libgit2-sys" "0.18.2+1.9.1"
                "08n223x2pkf4gj6yrjmh3z6q236qj6nifwww78xcblrbvw1zwhhw"))

(define rust-libgpg-error-sys-0.6.2
  ;; TODO: Check bundled sources.
  (crate-source "libgpg-error-sys" "0.6.2"
                "10kcif9qc4f6f52gdh5bvszg8vfxfjga2wygbc585v8n12y4q2jh"))

(define rust-libjmap-0.1.1
  (crate-source "libjmap" "0.1.1"
                "0y8n9f22a940hl02zaq166dmqszf0apmc1dbgcvybdkvz1g3b264"))

(define rust-libloading-0.7.4
  (crate-source "libloading" "0.7.4"
                "17wbccnjvhjd9ibh019xcd8kjvqws8lqgq86lqkpbgig7gyq0wxn"))

(define rust-libloading-0.8.6
  (crate-source "libloading" "0.8.6"
                "0d2ccr88f8kv3x7va2ccjxalcjnhrci4j2kwxp7lfmbkpjs4wbzw"))

(define rust-libloading-0.8.8
  (crate-source "libloading" "0.8.8"
                "0rw6q94psj3d6k0bi9nymqhyrz78lbdblryphhaszsw9p9ikj0q7"))

(define rust-libloading-0.8.9
  (crate-source "libloading" "0.8.9"
                "0mfwxwjwi2cf0plxcd685yxzavlslz7xirss3b9cbrzyk4hv1i6p"))

(define rust-libm-0.2.11
  (crate-source "libm" "0.2.11"
                "1yjgk18rk71rjbqcw9l1zaqna89p9s603k7n327nqs8dn88vwmc3"))

(define rust-libm-0.2.15
  (crate-source "libm" "0.2.15"
                "1plpzf0p829viazdj57yw5dhmlr8ywf3apayxc2f2bq5a6mvryzr"))

(define rust-libpulse-binding-2.28.1
  (crate-source "libpulse-binding" "2.28.1"
                "1zza12f22wf1qs6h71lq1i73aj3kmv3036hqc7qci063vyi5fdgd"))

(define rust-libpulse-binding-2.30.1
  (crate-source "libpulse-binding" "2.30.1"
                "15x3srbvqmwmhk20q9binq3cwakj59pgwnp61xl77qqnkq2b77lh"))

(define rust-libpulse-sys-1.21.0
  ;; TODO: Check bundled sources.
  (crate-source "libpulse-sys" "1.21.0"
                "16vs0qk6xadckb5qxlrhg0f4jn2zakfd7xih1lk1fb7lzc8f26dw"))

(define rust-libpulse-sys-1.22.0
  ;; TODO: Check bundled sources.
  (crate-source "libpulse-sys" "1.22.0"
                "1ljw5f2m4s99mp6m8s254iplfzr16qjscyhcdpiwfsjs0x2vzzmq"))

(define rust-librashader-cache-0.8.1
  (crate-source "librashader-cache" "0.8.1"
                "12zsdpgkiijacvldacanyvw43cc9s5fy4avq59x16c90p5is8rdd"))

(define rust-librashader-cache-0.9.2
  (crate-source "librashader-cache" "0.9.2"
                "0lyq8ijp9iydnssjiap24rcv7z99pyp1h0m7bii7zv1j19rvxs9f"))

(define rust-librashader-common-0.8.1
  (crate-source "librashader-common" "0.8.1"
                "0mb3d4k8s5lk8b75hx1dh5qs7bpiklngn8y42cgky5mhjxh4sdl7"))

(define rust-librashader-common-0.9.2
  (crate-source "librashader-common" "0.9.2"
                "09cnjcrddv20nnvhady84sg1fhgmv31f67hhxdykx429mjzwdhgf"))

(define rust-librashader-pack-0.8.1
  (crate-source "librashader-pack" "0.8.1"
                "175prw4xrnxj0rv7y00qxs93cds1zcz1jsyj0339dwkhd5jcaw7x"))

(define rust-librashader-pack-0.9.2
  (crate-source "librashader-pack" "0.9.2"
                "0gn2am1yf76vkclax3q0d3w8x9gs0m0sl8x7fqzwi7r38fixqv36"))

(define rust-librashader-preprocess-0.8.1
  (crate-source "librashader-preprocess" "0.8.1"
                "1yq588r4kl7472k6bzclqfz8fc3jwrmhwjmj4f9g58jpglnf7qa2"))

(define rust-librashader-preprocess-0.9.2
  (crate-source "librashader-preprocess" "0.9.2"
                "1nwhkjsfxya4s6hld1sqbn2f96jids7sb513alxg3856xg7flfid"))

(define rust-librashader-presets-0.8.1
  (crate-source "librashader-presets" "0.8.1"
                "0d76jdzzkl7sq7s1ql86qarb3zgbkw2fdyhpm5yx1yi2q4r3cdsp"))

(define rust-librashader-presets-0.9.2
  (crate-source "librashader-presets" "0.9.2"
                "1qpzb7qbav3k22qfpkc6a5chzig314ik50v79vvm449kx9i4h6cq"))

(define rust-librashader-reflect-0.8.1
  (crate-source "librashader-reflect" "0.8.1"
                "0jv09aagfa4q9i93xxxyhkh9j1p21q10f8vn20s0xvsgly7php4x"))

(define rust-librashader-reflect-0.9.2
  (crate-source "librashader-reflect" "0.9.2"
                "06jlyhkx9i8dlcxhwdayyhyrp0hgllgliqw1dlv68dwh661drza7"))

(define rust-librashader-runtime-0.8.1
  (crate-source "librashader-runtime" "0.8.1"
                "1lawijrdxsz34rmbnc9qqy25lvs8d846hmw7fiw1ld56niqgxhjp"))

(define rust-librashader-runtime-0.9.2
  (crate-source "librashader-runtime" "0.9.2"
                "0d290aw711bic7fzbjr7m5n685l8z64c0gvg2yvc2jis01ir1pgx"))

(define rust-libredox-0.1.10
  (crate-source "libredox" "0.1.10"
                "1jswil4ai90s4rh91fg8580x8nikni1zl3wnch4h01nvidqpwvs1"))

(define rust-libredox-0.1.12
  (crate-source "libredox" "0.1.12"
                "05h6fb2y05h74zwaafmnf7gv3bxilzp7syqlfzw524w55kh9a2rx"))

(define rust-libredox-0.1.3
  (crate-source "libredox" "0.1.3"
                "139602gzgs0k91zb7dvgj1qh4ynb8g1lbxsswdim18hcb6ykgzy0"))

(define rust-libredox-0.1.9
  (crate-source "libredox" "0.1.9"
                "1qqczzfqcc3sw3bl7la6qv7i9hy1s7sxhxmdvpxkfgdd3c9904ir"))

(define rust-libsqlite3-sys-0.30.1
  ;; TODO: Check bundled sources.
  (crate-source "libsqlite3-sys" "0.30.1"
                "0jcikvgbj84xc7ikdmpc8m4y5lyqgrb9aqblphwk67kv95xgp69f"))

(define rust-libsqlite3-sys-0.35.0
  ;; TODO: Check bundled sources.
  (crate-source "libsqlite3-sys" "0.35.0"
                "0gy1m6j1l94fxsirzp4h4rkrksf78rz7jy3px57qd1rcd8m1hg0k"))

(define rust-libssh2-sys-0.3.0
  ;; TODO: Check bundled sources.
  (crate-source "libssh2-sys" "0.3.0"
                "1vkidqw5ll71ynqc93hgcq62iqkklzb5268zffd13ql7nwqa1j1d"))

(define rust-libssh2-sys-0.3.1
  ;; TODO: Check bundled sources.
  (crate-source "libssh2-sys" "0.3.1"
                "1f8i31h3666rl6sq7v64ajdq03hmylkh6c1vaf9828aaml2ly3i2"))

(define rust-libtest-mimic-0.8.1
  (crate-source "libtest-mimic" "0.8.1"
                "0cwxnbv3wl00xsz7q0sf5xzksnqadqw8995a6g7a9pcyy4p9d5sj"))

(define rust-libudev-sys-0.1.4
  ;; TODO: Check bundled sources.
  (crate-source "libudev-sys" "0.1.4"
                "09236fdzlx9l0dlrsc6xx21v5x8flpfm3d5rjq9jr5ivlas6k11w"))

(define rust-libz-ng-sys-1.1.22
  ;; TODO: Check bundled sources.
  (crate-source "libz-ng-sys" "1.1.22"
                "096qkwzy596zf88nfppr2vbw9fbjfr81k2ws4zf6wyrw58n8q4d7"))

(define rust-libz-rs-sys-0.5.2
  ;; TODO: Check bundled sources.
  (crate-source "libz-rs-sys" "0.5.2"
                "1kdy093bhxfkgx7li3raxigcc3qdqjn3hvrpjkblvv6r777vh3c4"))

(define rust-libz-sys-1.1.20
  ;; TODO: Check bundled sources.
  (crate-source "libz-sys" "1.1.20"
                "0wp4i6zl385ilmcqafv61jwsk1mpk6yb8gpws9nwza00x19n9lfj"))

(define rust-libz-sys-1.1.21
  ;; TODO: Check bundled sources.
  (crate-source "libz-sys" "1.1.21"
                "1ajfpf413j9m7kmf4fwvvgv5jxxm5s438f2pfbv2c2vf1vjni6yz"))

(define rust-libz-sys-1.1.22
  ;; TODO: Check bundled sources.
  (crate-source "libz-sys" "1.1.22"
                "07b5wxh0ska996kc0g2hanjhmb4di7ksm6ndljhr4pi0vykyfw4b"))

(define rust-libz-sys-1.1.23
  ;; TODO: Check bundled sources.
  (crate-source "libz-sys" "1.1.23"
                "1xy0l46gmhfyk28r9pm7njl0h7dh8l2vpiqw67kn043pyfxiil8m"))

(define rust-line-clipping-0.3.5
  (crate-source "line-clipping" "0.3.5"
                "0jlakbyjc5sh4j2lx2glyjar3wqq9mqifkdzbhvhkgyxk17f8kaz"))

(define rust-linked-hash-map-0.5.6
  (crate-source "linked-hash-map" "0.5.6"
                "03vpgw7x507g524nx5i1jf5dl8k3kv0fzg8v3ip6qqwbpkqww5q7"))

(define rust-linux-keyutils-0.2.4
  (crate-source "linux-keyutils" "0.2.4"
                "13nipvk2mzk76y7yfsqwnwsqk21x6xy8fkmqz5is99fqbzn4j7kn"))

(define rust-linux-raw-sys-0.11.0
  ;; TODO: Check bundled sources.
  (crate-source "linux-raw-sys" "0.11.0"
                "0fghx0nn8nvbz5yzgizfcwd6ap2pislp68j8c1bwyr6sacxkq7fz"))

(define rust-linux-raw-sys-0.3.8
  ;; TODO: Check bundled sources.
  (crate-source "linux-raw-sys" "0.3.8"
                "068mbigb3frrxvbi5g61lx25kksy98f2qgkvc4xg8zxznwp98lzg"))

(define rust-linux-raw-sys-0.4.12
  ;; TODO: Check bundled sources.
  (crate-source "linux-raw-sys" "0.4.12"
                "0mhlla3gk1jgn6mrq9s255rvvq8a1w3yk2vpjiwsd6hmmy1imkf4"))

(define rust-linux-raw-sys-0.4.14
  ;; TODO: Check bundled sources.
  (crate-source "linux-raw-sys" "0.4.14"
                "12gsjgbhhjwywpqcrizv80vrp7p7grsz5laqq773i33wphjsxcvq"))

(define rust-linux-raw-sys-0.4.15
  ;; TODO: Check bundled sources.
  (crate-source "linux-raw-sys" "0.4.15"
                "1aq7r2g7786hyxhv40spzf2nhag5xbw2axxc1k8z5k1dsgdm4v6j"))

(define rust-linux-raw-sys-0.4.5
  ;; TODO: Check bundled sources.
  (crate-source "linux-raw-sys" "0.4.5"
                "00w52pb2cb4b2880ksyzagmzbyjdmp9ac0w3qfvjv3453fnzvg2p"))

(define rust-linux-raw-sys-0.6.5
  ;; TODO: Check bundled sources.
  (crate-source "linux-raw-sys" "0.6.5"
                "1mv3c1zz51ydcj768zavm8g06gz5jb1p7yigmmif7hz5whdmnf1a"))

(define rust-linux-raw-sys-0.9.3
  ;; TODO: Check bundled sources.
  (crate-source "linux-raw-sys" "0.9.3"
                "04zl7j4k1kgbn7rrl3i7yszaglgxp0c8dbwx8f1cabnjjwhb2zgy"))

(define rust-linux-raw-sys-0.9.4
  ;; TODO: Check bundled sources.
  (crate-source "linux-raw-sys" "0.9.4"
                "04kyjdrq79lz9ibrf7czk6cv9d3jl597pb9738vzbsbzy1j5i56d"))

(define rust-litemap-0.7.4
  (crate-source "litemap" "0.7.4"
                "012ili3vppd4952sh6y3qwcd0jkd0bq2qpr9h7cppc8sj11k7saf"))

(define rust-litemap-0.7.5
  (crate-source "litemap" "0.7.5"
                "0mi8ykav0s974ps79p438x04snh0cdb7lc864b42jws5375i9yr3"))

(define rust-litemap-0.8.0
  (crate-source "litemap" "0.8.0"
                "0mlrlskwwhirxk3wsz9psh6nxcy491n0dh8zl02qgj0jzpssw7i4"))

(define rust-litemap-0.8.1
  (crate-source "litemap" "0.8.1"
                "0xsy8pfp9s802rsj1bq2ys2kbk1g36w5dr3gkfip7gphb5x60wv3"))

(define rust-litrs-0.4.2
  (crate-source "litrs" "0.4.2"
                "1v8bxsrkm0w2k9nmbp8hsspy9i1lawajywqdw4hx87rjzqv41rgm"))

(define rust-litrs-1.0.0
  (crate-source "litrs" "1.0.0"
                "14p0kzzkavnngvybl88nvfwv031cc2qx4vaxpfwsiifm8grdglqi"))

(define rust-local-channel-0.1.5
  (crate-source "local-channel" "0.1.5"
                "1j1ywn459kl4fdmjfyljm379k40qwwscd7mqp25lppxqd5gcijxn"))

(define rust-local-waker-0.1.4
  (crate-source "local-waker" "0.1.4"
                "11vlcm8q6dhdf0srkgjnwca48dn9zcz820fq20hv82ffcxy3v1sd"))

(define rust-locale-config-0.3.0
  (crate-source "locale_config" "0.3.0"
                "0d399alr1i7h7yji4vydbdbzd8hp0xaykr7h4rn3yj7l2rdw7lh8"))

(define rust-lock-api-0.4.10
  (crate-source "lock_api" "0.4.10"
                "05nd9nzxqidg24d1k8y5vlc8lz9gscpskrikycib46qbl8brgk61"))

(define rust-lock-api-0.4.11
  (crate-source "lock_api" "0.4.11"
                "0iggx0h4jx63xm35861106af3jkxq06fpqhpkhgw0axi2n38y5iw"))

(define rust-lock-api-0.4.12
  (crate-source "lock_api" "0.4.12"
                "05qvxa6g27yyva25a5ghsg85apdxkvr77yhkyhapj6r8vnf8pbq7"))

(define rust-lock-api-0.4.13
  (crate-source "lock_api" "0.4.13"
                "0rd73p4299mjwl4hhlfj9qr88v3r0kc8s1nszkfmnq2ky43nb4wn"))

(define rust-lock-api-0.4.14
  (crate-source "lock_api" "0.4.14"
                "0rg9mhx7vdpajfxvdjmgmlyrn20ligzqvn8ifmaz7dc79gkrjhr2"))

(define rust-log-0.3.9
  (crate-source "log" "0.3.9"
                "0jq23hhn5h35k7pa8r7wqnsywji6x3wn1q5q7lif5q536if8v7p1"))

(define rust-log-0.4.17
  (crate-source "log" "0.4.17"
                "0biqlaaw1lsr8bpnmbcc0fvgjj34yy79ghqzyi0ali7vgil2xcdb"))

(define rust-log-0.4.20
  (crate-source "log" "0.4.20"
                "13rf7wphnwd61vazpxr7fiycin6cb1g8fmvgqg18i464p0y1drmm"))

(define rust-log-0.4.21
  (crate-source "log" "0.4.21"
                "074hldq1q8rlzq2s2qa8f25hj4s3gpw71w64vdwzjd01a4g8rvch"))

(define rust-log-0.4.22
  (crate-source "log" "0.4.22"
                "093vs0wkm1rgyykk7fjbqp2lwizbixac1w52gv109p5r4jh0p9x7"))

(define rust-log-0.4.24
  (crate-source "log" "0.4.24"
                "038sdr8l758wi24igjvyx0pwk51jr5rgrmx767p30h10ijja4vix"))

(define rust-log-0.4.26
  (crate-source "log" "0.4.26"
                "17mvchkvhnm2zxyfagh2g9p861f0qx2g1sg2v14sww9nvjry5g9h"))

(define rust-log-0.4.27
  (crate-source "log" "0.4.27"
                "150x589dqil307rv0rwj0jsgz5bjbwvl83gyl61jf873a7rjvp0k"))

(define rust-log-0.4.28
  (crate-source "log" "0.4.28"
                "0cklpzrpxafbaq1nyxarhnmcw9z3xcjrad3ch55mmr58xw2ha21l"))

(define rust-log-0.4.29
  (crate-source "log" "0.4.29"
                "15q8j9c8g5zpkcw0hnd6cf2z7fxqnvsjh3rw5mv5q10r83i34l2y"))

(define rust-logos-0.15.0
  (crate-source "logos" "0.15.0"
                "1nzkjsi7ckhqkgq9w8baw7hyg5lq0bwa2ggpxn0wrizl39n56vxb"))

(define rust-logos-0.15.1
  (crate-source "logos" "0.15.1"
                "0m41xcjn6yh3v18618v9f04v7vkmf3zn07y5c68xkhjfkf4jyizz"))

(define rust-logos-codegen-0.15.0
  (crate-source "logos-codegen" "0.15.0"
                "0kmkmyd42rsxd04avmr2z3jdnblg81v94pkyg7manc0knv8bz6qq"))

(define rust-logos-codegen-0.15.1
  (crate-source "logos-codegen" "0.15.1"
                "0p04jfvaaiw2rj4kzk1s4hlmwhbwvgn3xi5jl0kmph5hj0mklahr"))

(define rust-logos-derive-0.15.0
  (crate-source "logos-derive" "0.15.0"
                "1fjwbpxr4yvwp8nhraqbp62ipq8mn8schjnirgydp78434d8xzpb"))

(define rust-logos-derive-0.15.1
  (crate-source "logos-derive" "0.15.1"
                "0w5l4qm67b551pnx3dksbyia9mm339a53z4fsd13mvympjbrcpb0"))

(define rust-loop9-0.1.5
  (crate-source "loop9" "0.1.5"
                "0qphc1c0cbbx43pwm6isnwzwbg6nsxjh7jah04n1sg5h4p0qgbhg"))

(define rust-lru-0.12.4
  (crate-source "lru" "0.12.4"
                "017rzh4kyl3j79sj0qc35wallblsqbnkzxpn6i3xkrv02y4kkvip"))

(define rust-lru-0.12.5
  (crate-source "lru" "0.12.5"
                "0f1a7cgqxbyhrmgaqqa11m3azwhcc36w0v5r4izgbhadl3sg8k13"))

(define rust-lru-0.16.0
  (crate-source "lru" "0.16.0"
                "1vda21xn26iz5qdy02fbl14sn7wz926iizyac8w2zxbz11jlxsl6"))

(define rust-lru-0.16.1
  (crate-source "lru" "0.16.1"
                "1j3ffpdz734s75ish396165gv4yx1zihp6isif9vpyj6kwc4ksdz"))

(define rust-lru-0.16.2
  (crate-source "lru" "0.16.2"
                "03x2rmjg1zjb4yl5460raldn7bxr6zpn0f929b6wjg8qzi31n1cn"))

(define rust-lru-0.16.3
  (crate-source "lru" "0.16.3"
                "14z5yxcp3f63lgw8yxr486g9yz7cfqbmkadfwgw36vy0jbslgp51"))

(define rust-lru-cache-0.1.2
  (crate-source "lru-cache" "0.1.2"
                "071viv6g2p3akwqmfb3c8vsycs5n7kr17b70l7la071jv0d4zqii"))

(define rust-lru-slab-0.1.2
  (crate-source "lru-slab" "0.1.2"
                "0m2139k466qj3bnpk66bwivgcx3z88qkxvlzk70vd65jq373jaqi"))

(define rust-lsp-types-0.94.1
  (crate-source "lsp-types" "0.94.1"
                "18aym1qfgyaf9f9504yz911gssck5rv190izzr3hdqbal12gssy6"))

(define rust-lua-src-547.0.0
  ;; TODO: Check bundled sources.
  (crate-source "lua-src" "547.0.0"
                "0hjy276xbidvp8n0c6f1w76qamybiijfa0b7fj5rpd0p6ngg5nhy"))

(define rust-lua-src-548.1.2
  ;; TODO: Check bundled sources.
  (crate-source "lua-src" "548.1.2"
                "1a4cm1zlpdxasw81ak3z63i2pg6d7mh1jizvrw45zb92yjpy3i5x"))

(define rust-lurk-cli-0.3.11
  (crate-source "lurk-cli" "0.3.11"
                "1j05qqcs4zwpi68skkfc2lxnrfi8pys155jh7kcjpxjal9d6ypqg"))

(define rust-lyon-1.0.1
  (crate-source "lyon" "1.0.1"
                "0kx7wa804gssgm3gqvg2ywdijzbcn2bm3jk3ks030m4bm76zkrwi"))

(define rust-lyon-1.0.16
  (crate-source "lyon" "1.0.16"
                "0lj3xz8ypbrkam7a80qyrryq2mk60qnr0hcx9hv3g2acsma7vjyv"))

(define rust-lyon-algorithms-1.0.16
  (crate-source "lyon_algorithms" "1.0.16"
                "0ixnnsm59pg0gh2gwzq5vgk6rqc77468a395dwwkdwy452g85h7l"))

(define rust-lyon-algorithms-1.0.5
  (crate-source "lyon_algorithms" "1.0.5"
                "021wyk5jm29fcad3qbpazhm4pawfis2ys206wwvpsz15kphrng7i"))

(define rust-lyon-geom-1.0.17
  (crate-source "lyon_geom" "1.0.17"
                "1rzjdjikff28z04ra438a6k0g0j03q80kln2q6q4hy0cfq6pf5jf"))

(define rust-lyon-geom-1.0.6
  (crate-source "lyon_geom" "1.0.6"
                "0w45pmyn5bpyk39wyrgniam08z3dpg23dr1yid1xywkj13f9xxla"))

(define rust-lyon-path-1.0.16
  (crate-source "lyon_path" "1.0.16"
                "1szx0vsm83wbxwafhmdzf051x0gvkx9rn0mshics2cnnrxmsiv0s"))

(define rust-lyon-path-1.0.6
  (crate-source "lyon_path" "1.0.6"
                "0bi54pwmpyaw0xswa5s4zwx3mjxpm6wqay93xxp6yn2q5zn8l2wf"))

(define rust-lyon-tessellation-1.0.15
  (crate-source "lyon_tessellation" "1.0.15"
                "0b0vjphcpwsghxjzm6s3qzbcg5l6af7z5vigzxp882ab18v457ap"))

(define rust-lyon-tessellation-1.0.16
  (crate-source "lyon_tessellation" "1.0.16"
                "0lx39aq442pgfhdspsa3zh40zn3xr7vkk5f83ddk700j5qa8dxgk"))

(define rust-mac-address-1.1.7
  (crate-source "mac_address" "1.1.7"
                "13m0jdaaq77r8azml00dm5ca1ym253l7kpgw9s5jrgnls3lzldl8"))

(define rust-mac-address-1.1.8
  (crate-source "mac_address" "1.1.8"
                "00r3n18mxglq1dzshnm0vxk1fgsp3c2hd08w6hfcqdp8ymmv5bn0"))

(define rust-macaddr-1.0.1
  (crate-source "macaddr" "1.0.1"
                "1n5jxn79krlql810c4w3hdkvyqc01141dc5y6fr9sxff2yy0pvms"))

(define rust-mach2-0.4.2
  (crate-source "mach2" "0.4.2"
                "02gpyq89rcrqdbz4hgp5bpjas21dllxfc70jgw8vj0iaxg6mbf8r"))

(define rust-mach2-0.4.3
  (crate-source "mach2" "0.4.3"
                "0i6vcnbq5v51whgyidzhf7cbxqrmj2nkw8z0m2ib02rc60mjhh6n"))

(define rust-macro-rules-attribute-0.2.0
  (crate-source "macro_rules_attribute" "0.2.0"
                "04waa4qm28adwnxsxhx9135ki68mwkikr6m5pi5xhcy0gcgjg0la"))

(define rust-macro-rules-attribute-proc-macro-0.2.0
  (crate-source "macro_rules_attribute-proc_macro" "0.2.0"
                "0s45j4zm0a5d041g3vcbanvr76p331dfjb7gw9qdmh0w8mnqbpdq"))

(define rust-mail-builder-0.3.2
  (crate-source "mail-builder" "0.3.2"
                "1jzg9y92xbdj2glkpbakhrgv0scd1ih9a2vmxvr81vbha8fqgx95"))

(define rust-mail-builder-0.4.4
  (crate-source "mail-builder" "0.4.4"
                "0fs7214f92cav734hi0n9cr6fh3q1dv4vccal89l131k0zrrh2ch"))

(define rust-mail-parser-0.11.1
  (crate-source "mail-parser" "0.11.1"
                "18qra6c5fcvjif1r3pjj553bnpc1h2sxzk6y1crzmrn4843kkx6w"))

(define rust-mail-parser-0.9.4
  (crate-source "mail-parser" "0.9.4"
                "1z7r9b4fn852s3kqi2mzlg01isfn6wxw9frh6dbsyzxiv3jvkhwk"))

(define rust-mail-send-0.4.9
  (crate-source "mail-send" "0.4.9"
                "0m35h72izqs5gga8axqqqs8yjira4hlkniqbg7jniv80rwjmsmvs"))

(define rust-maildirs-0.2.2
  (crate-source "maildirs" "0.2.2"
                "1xmf16bv2gsk3hwd6nydsi98lhrbwjifv8m5f6j1cv08dydxz58c"))

(define rust-malloc-buf-0.0.6
  (crate-source "malloc_buf" "0.0.6"
                "1jqr77j89pwszv51fmnknzvd53i1nkmcr8rjrvcxhm4dx1zr1fv2"))

(define rust-maplit-1.0.2
  (crate-source "maplit" "1.0.2"
                "07b5kjnhrrmfhgqm9wprjw8adx6i225lqp49gasgqg74lahnabiy"))

(define rust-match-cfg-0.1.0
  (crate-source "match_cfg" "0.1.0"
                "1r5j3zqc3qr8ybcx95bk8q57mkizmgmffj5lmicd4i8d9riyigpz"))

(define rust-matchers-0.1.0
  (crate-source "matchers" "0.1.0"
                "0n2mbk7lg2vf962c8xwzdq96yrc9i0p8dbmm4wa1nnkcp1dhfqw2"))

(define rust-matchers-0.2.0
  (crate-source "matchers" "0.2.0"
                "1sasssspdj2vwcwmbq3ra18d3qniapkimfcbr47zmx6750m5llni"))

(define rust-matchit-0.8.4
  (crate-source "matchit" "0.8.4"
                "1hzl48fwq1cn5dvshfly6vzkzqhfihya65zpj7nz7lfx82mgzqa7"))

(define rust-maxminddb-0.24.0
  (crate-source "maxminddb" "0.24.0"
                "0g3w2addx96r9zn8gbhgxz9rhdxyqzxklmvzgjxn2j51irfpw26n"))

(define rust-maxminddb-0.26.0
  (crate-source "maxminddb" "0.26.0"
                "1fhbi3qlfld2v4jg5qagjnsdky4vnnq78v20ha38b2176927w69a"))

(define rust-maybe-async-0.2.10
  (crate-source "maybe-async" "0.2.10"
                "04fvg2ywb2p9dzf7i35xqfibxc05k1pirv36jswxcqg3qw82ryaw"))

(define rust-maybe-rayon-0.1.1
  (crate-source "maybe-rayon" "0.1.1"
                "06cmvhj4n36459g327ng5dnj8d58qs472pv5ahlhm7ynxl6g78cf"))

(define rust-mccs-0.1.3
  (crate-source "mccs" "0.1.3"
                "1kc8345bgacw5bl9hrbcxwz4z4m1vaf6n2k6icaysbylvsrxd430"))

(define rust-mccs-caps-0.1.3
  (crate-source "mccs-caps" "0.1.3"
                "1560krp2swrjd1lp5a3pzbzd0cccp0mbwxm2rxlpkc1v3b863fcf"))

(define rust-mccs-db-0.1.3
  (crate-source "mccs-db" "0.1.3"
                "0f4xyilm4szclvh4vzqx85qvqn0hvjxwpf61xj5r38d137zainiw"))

(define rust-md-5-0.10.6
  (crate-source "md-5" "0.10.6"
                "1kvq5rnpm4fzwmyv5nmnxygdhhb2369888a06gdc9pxyrzh7x7nq"))

(define rust-md5-0.7.0
  (crate-source "md5" "0.7.0"
                "0wcps37hrhz59fkhf8di1ppdnqld6l1w5sdy7jp7p51z0i4c8329"))

(define rust-memchr-1.0.2
  (crate-source "memchr" "1.0.2"
                "0yjyja34pzhipdl855q3m21w1lyih4lw79x2dp3czwdla4pap3ql"))

(define rust-memchr-2.6.2
  (crate-source "memchr" "2.6.2"
                "0vnqrsfm5260gcxyb83ipfd68d51l3azpm81i8dyc6320b8ax1jl"))

(define rust-memchr-2.6.3
  (crate-source "memchr" "2.6.3"
                "0p6kn2awqf47m3brk0nmajarhwhylg9969il8dm9bq87yxp2s8wg"))

(define rust-memchr-2.7.1
  (crate-source "memchr" "2.7.1"
                "0jf1kicqa4vs9lyzj4v4y1p90q0dh87hvhsdd5xvhnp527sw8gaj"))

(define rust-memchr-2.7.4
  (crate-source "memchr" "2.7.4"
                "18z32bhxrax0fnjikv475z7ii718hq457qwmaryixfxsl2qrmjkq"))

(define rust-memchr-2.7.5
  (crate-source "memchr" "2.7.5"
                "1h2bh2jajkizz04fh047lpid5wgw2cr9igpkdhl3ibzscpd858ij"))

(define rust-memchr-2.7.6
  (crate-source "memchr" "2.7.6"
                "0wy29kf6pb4fbhfksjbs05jy2f32r2f3r1ga6qkmpz31k79h0azm"))

(define rust-memmap2-0.5.10
  (crate-source "memmap2" "0.5.10"
                "09xk415fxyl4a9pgby4im1v2gqlb5lixpm99dczkk30718na9yl3"))

(define rust-memmap2-0.9.0
  (crate-source "memmap2" "0.9.0"
                "0xckkh1i45g6y2g2lkb6b292pfj2wlrfk2fc4754q7dzga6s7ayy"))

(define rust-memmap2-0.9.5
  (crate-source "memmap2" "0.9.5"
                "0krpvvkpg4i3l05cv3q2xk24a1vj5c86gbrli2wzhj1qkpnpwgzx"))

(define rust-memmap2-0.9.7
  (crate-source "memmap2" "0.9.7"
                "0a1d05ci4z7bppkdmsf5j1inlgp9sx0jphg50pn4qwrx62nmhds8"))

(define rust-memmap2-0.9.8
  (crate-source "memmap2" "0.9.8"
                "1dqxjs89krh8cin0k7ksqc9myw9yni9kn8d8cllwq4fn1isrhfl4"))

(define rust-memmap2-0.9.9
  (crate-source "memmap2" "0.9.9"
                "146lfx0mpib44wvws6hibahm4h2w867bzwsc6zhmi9p0l3j36hbl"))

(define rust-memmem-0.1.1
  (crate-source "memmem" "0.1.1"
                "05ccifqgxdfxk6yls41ljabcccsz3jz6549l1h3cwi17kr494jm6"))

(define rust-memoffset-0.7.1
  (crate-source "memoffset" "0.7.1"
                "1x2zv8hv9c9bvgmhsjvr9bymqwyxvgbca12cm8xkhpyy5k1r7s2x"))

(define rust-memoffset-0.8.0
  (crate-source "memoffset" "0.8.0"
                "1qcdic88dhgw76pafgndpz04pig8il4advq978mxdxdwrydp276n"))

(define rust-memoffset-0.9.0
  (crate-source "memoffset" "0.9.0"
                "0v20ihhdzkfw1jx00a7zjpk2dcp5qjq6lz302nyqamd9c4f4nqss"))

(define rust-memoffset-0.9.1
  (crate-source "memoffset" "0.9.1"
                "12i17wh9a9plx869g7j4whf62xw68k5zd4k0k5nh6ys5mszid028"))

(define rust-metal-0.27.0
  (crate-source "metal" "0.27.0"
                "09bz461vyi9kw69k55gy2fpd3hz17j6g2n0v08gm3glc7yap6gy4"))

(define rust-metal-0.31.0
  (crate-source "metal" "0.31.0"
                "17pr30nl0djac4qvwjh3fj9wwc4ic8cwn4wqsqx79dchcjagnsgm"))

(define rust-metal-0.32.0
  (crate-source "metal" "0.32.0"
                "01g6a10l2p576ibybz0v9hignw7qj0r6d513qbf2vxrzcxpmmh80"))

(define rust-mime-0.3.17
  (crate-source "mime" "0.3.17"
                "16hkibgvb9klh0w0jk5crr5xv90l3wlf77ggymzjmvl1818vnxv8"))

(define rust-mime-guess-2.0.5
  (crate-source "mime_guess" "2.0.5"
                "03jmg3yx6j39mg0kayf7w4a886dl3j15y8zs119zw01ccy74zi7p"))

(define rust-minicov-0.3.7
  (crate-source "minicov" "0.3.7"
                "0jsvi62lklfyvdmsiizipkqcfpsc7h4c4illgxlf28iwrkqyjzzj"))

(define rust-minicov-0.3.8
  (crate-source "minicov" "0.3.8"
                "0kg2bajhhzwafpvhqpvfaddrwy6z0ggvqlirdpb0b5jnj6jbcsa8"))

(define rust-minimad-0.13.1
  (crate-source "minimad" "0.13.1"
                "1lj5szpri8hjf38qrmg0i7vabp9b1rwakm5nly86a63d484dgid9"))

(define rust-minimal-lexical-0.2.1
  (crate-source "minimal-lexical" "0.2.1"
                "16ppc5g84aijpri4jzv14rvcnslvlpphbszc7zzp6vfkddf4qdb8"))

(define rust-miniz-oxide-0.6.2
  (crate-source "miniz_oxide" "0.6.2"
                "1yp8z6yll5ypz1ldmgnv7zi0r78kbvmqmn2mii77jzmk5069axdj"))

(define rust-miniz-oxide-0.7.1
  (crate-source "miniz_oxide" "0.7.1"
                "1ivl3rbbdm53bzscrd01g60l46lz5krl270487d8lhjvwl5hx0g7"))

(define rust-miniz-oxide-0.7.2
  (crate-source "miniz_oxide" "0.7.2"
                "19qlxb21s6kabgqq61mk7kd1qk2invyygj076jz6i1gj2lz1z0cx"))

(define rust-miniz-oxide-0.7.4
  (crate-source "miniz_oxide" "0.7.4"
                "024wv14aa75cvik7005s5y2nfc8zfidddbd7g55g7sjgnzfl18mq"))

(define rust-miniz-oxide-0.8.2
  (crate-source "miniz_oxide" "0.8.2"
                "1543asrvhla92sby4z6m9ilkg2cmmq8ja6bj84k1vp6f48qfiysg"))

(define rust-miniz-oxide-0.8.5
  (crate-source "miniz_oxide" "0.8.5"
                "1r9whkc61xri7m1cn4rjrjlhr32ab29nvfxcbg0ri5mmpgg08glf"))

(define rust-miniz-oxide-0.8.7
  (crate-source "miniz_oxide" "0.8.7"
                "0c4lj692adnzw0h9j8l24d7imds3icpgdkk3b03zlhxf90zcww7z"))

(define rust-miniz-oxide-0.8.8
  (crate-source "miniz_oxide" "0.8.8"
                "0al9iy33flfgxawj789w2c8xxwg1n2r5vv6m6p5hl2fvd2vlgriv"))

(define rust-miniz-oxide-0.8.9
  (crate-source "miniz_oxide" "0.8.9"
                "05k3pdg8bjjzayq3rf0qhpirq9k37pxnasfn4arbs17phqn6m9qz"))

(define rust-mio-0.8.10
  (crate-source "mio" "0.8.10"
                "02gyaxvaia9zzi4drrw59k9s0j6pa5d1y2kv7iplwjipdqlhngcg"))

(define rust-mio-0.8.11
  (crate-source "mio" "0.8.11"
                "034byyl0ardml5yliy1hmvx8arkmn9rv479pid794sm07ia519m4"))

(define rust-mio-1.0.1
  (crate-source "mio" "1.0.1"
                "1r5g65s5acsx440m0a3pylclbrd0dqz93hg15k9crpllsdbf8sa5"))

(define rust-mio-1.0.2
  (crate-source "mio" "1.0.2"
                "1v1cnnn44awxbcfm4zlavwgkvbyg7gp5zzjm8mqf1apkrwflvq40"))

(define rust-mio-1.0.3
  (crate-source "mio" "1.0.3"
                "1gah0h4ia3avxbwym0b6bi6lr6rpysmj9zvw6zis5yq0z0xq91i8"))

(define rust-mio-1.0.4
  (crate-source "mio" "1.0.4"
                "073n3kam3nz8j8had35fd2nn7j6a33pi3y5w3kq608cari2d9gkq"))

(define rust-mio-1.1.0
  (crate-source "mio" "1.1.0"
                "0wr816q3jrjwiajvw807lgi540i9s6r78a5fx4ycz3nwhq03pn39"))

(define rust-mio-1.1.1
  (crate-source "mio" "1.1.1"
                "1z2phpalqbdgihrcjp8y09l3kgq6309jnhnr6h11l9s7mnqcm6x6"))

(define rust-miow-0.5.0
  (crate-source "miow" "0.5.0"
                "08qi8xm2zf8dqacdbnrp19aqk2xiwmw75n1mpq43rqsmysibrzsj"))

(define rust-miow-0.6.0
  (crate-source "miow" "0.6.0"
                "0i307jyhxnhgzj148cdb9zq59rhlhr1b65g142g9z9r01d1pd7rm"))

(define rust-miow-0.6.1
  (crate-source "miow" "0.6.1"
                "023g6jamln6hmspcllgrm99ri38ypbm8w945jh36579hgb9zlssk"))

(define rust-mlua-0.11.5
  (crate-source "mlua" "0.11.5"
                "0crb3wslj8hac0b2j6zyfxgmalp0b1ryndw137bzqzlh75swcnlk"))

(define rust-mlua-0.9.9
  (crate-source "mlua" "0.9.9"
                "1iqxq7lnv7si4ydglsr73nqglfsj893ryc212lzd76wwiaqxw4fi"))

(define rust-mlua-sys-0.6.1
  ;; TODO: Check bundled sources.
  (crate-source "mlua-sys" "0.6.1"
                "1g4azbmsbps486q91cbq70gm0g6291fqq0dsd7snfxfz4c3yv250"))

(define rust-mlua-sys-0.9.0
  ;; TODO: Check bundled sources.
  (crate-source "mlua-sys" "0.9.0"
                "1n0av9mhh22n388dx4857ng4qvpq2vp8aywfrb4rzcgn3gr8m5lc"))

(define rust-mml-lib-1.1.1
  (crate-source "mml-lib" "1.1.1"
                "14656qwl8bhp927209g4kspzqja3vyk3kzam81rlfjx3j02cprrr"))

(define rust-mockall-0.13.1
  (crate-source "mockall" "0.13.1"
                "1lir70dd9cnsjlf20gi3i51ha9n7mlrkx74bx5gfszlcdk6bz9ir"))

(define rust-mockall-derive-0.13.1
  (crate-source "mockall_derive" "0.13.1"
                "1608qajqrz23xbvv81alc6wm4l24as1bsqg4shdh3sggq8231ji5"))

(define rust-moka-0.12.11
  (crate-source "moka" "0.12.11"
                "0xxhhs7lsfk47q065vibvggjhrf6h0lsvfji3m601q0jqf4csqc2"))

(define rust-moka-0.12.12
  (crate-source "moka" "0.12.12"
                "0apf9r7lz2fc0r8fmwynaiqc39lk70vrkzaqigh492dh66ywdpm3"))

(define rust-moka-0.12.8
  (crate-source "moka" "0.12.8"
                "0vrbsd86bdnliwgnzwqw6gi3x7n4fl8gnck4wzfx4xfr9pmn5krj"))

(define rust-moxcms-0.7.10
  (crate-source "moxcms" "0.7.10"
                "022njiwc0843877ra5ahhi2jhm8jhqyn29jcwlxvspljryxnp640"))

(define rust-moxcms-0.7.5
  (crate-source "moxcms" "0.7.5"
                "026df3qpxn430dlngpj3gjip0m9280g3asvbia5dpsjsjfl2zlyx"))

(define rust-moxcms-0.7.9
  (crate-source "moxcms" "0.7.9"
                "1xkfc6cd81rvsm2yijjsph0gmw0z4waa2zlb5f4mx2vg8gbx7g8g"))

(define rust-mpd-client-1.3.0
  (crate-source "mpd_client" "1.3.0"
                "0c1kjgkh69lamacmjkrvcbbw2mbaqr6cqwhcsmryfgq6jmnxpm6h"))

(define rust-mpd-client-1.4.1
  (crate-source "mpd_client" "1.4.1"
                "13mgkswdndxvjzjvmy8q64czrilcfn9dihcqc74fafjwgxghbhir"))

(define rust-mpd-protocol-1.0.1
  (crate-source "mpd_protocol" "1.0.1"
                "0fhyp5dqh0xax8cskpclr6phzs8cwb3nvky895m2bjx4y15bq0dd"))

(define rust-mpd-protocol-1.0.3
  (crate-source "mpd_protocol" "1.0.3"
                "1hmcrdyml1kzn39xiil56xdgvfkx2ldz67w62608g9rfnr1l35j0"))

(define rust-mpd-utils-0.2.1
  (crate-source "mpd-utils" "0.2.1"
                "1ljlybw28l21f55s1ymg5c3xqlq9lsd595jyz7plcsi0ndqhxrcg"))

(define rust-mpris-2.0.1
  (crate-source "mpris" "2.0.1"
                "1svzblilq3kxgra63axl3wibsnl9g8p6b8q1x401wsw2lxazkkjm"))

(define rust-mutate-once-0.1.2
  (crate-source "mutate_once" "0.1.2"
                "1byj4yz6h0mdx3mmj0mq23ma59kw41pckspr2gz8rl22k0y27lhk"))

(define rust-naga-0.19.2
  (crate-source "naga" "0.19.2"
                "0hq82fg8rj067wppqrk7h9q5b89vkp98spmb37s9lggm89355qsh"))

(define rust-naga-25.0.1
  (crate-source "naga" "25.0.1"
                "0cm69nrd703v5slnk397rd16k0wb7cf678xcz5brgr16bx27r5rb"))

(define rust-naga-27.0.3
  (crate-source "naga" "27.0.3"
                "1n57m5hwi42mk57ljz7manc45b8ky8890891y86yw4cb1rgz4v06"))

(define rust-nanohtml2text-0.1.5
  (crate-source "nanohtml2text" "0.1.5"
                "0ryqldqv3668k0658nyp9rngbb59xbqxhfrlh2i2skbm8lvdqjwx"))

(define rust-native-tls-0.2.11
  (crate-source "native-tls" "0.2.11"
                "0bmrlg0fmzxaycjpkgkchi93av07v2yf9k33gc12ca9gqdrn28h7"))

(define rust-native-tls-0.2.12
  (crate-source "native-tls" "0.2.12"
                "0rkl65z70n7sy4d5w0qa99klg1hr43wx6kcprk4d2n9xr2r4wqd8"))

(define rust-native-tls-0.2.14
  (crate-source "native-tls" "0.2.14"
                "03hga800x8bzkp8h7frnm7yp545dwwawgmaq673vx7byk1139pl7"))

(define rust-ndk-0.8.0
  (crate-source "ndk" "0.8.0"
                "1dx5yyqh32bi161mipg4br4i33syjidw81qrq0w7mc8hf0ds6xi0"))

(define rust-ndk-0.9.0
  (crate-source "ndk" "0.9.0"
                "1m32zpmi5w1pf3j47k6k5fw395dc7aj8d0mdpsv53lqkprxjxx63"))

(define rust-ndk-context-0.1.1
  (crate-source "ndk-context" "0.1.1"
                "12sai3dqsblsvfd1l1zab0z6xsnlha3xsfl7kagdnmj3an3jvc17"))

(define rust-ndk-sys-0.5.0+25.2.9519653
  ;; TODO: Check bundled sources.
  (crate-source "ndk-sys" "0.5.0+25.2.9519653"
                "14bnxww0f17xl8pyn6j5kpkl98snjl9lin8i7qv4zzb0vmlnf6cc"))

(define rust-ndk-sys-0.6.0+11769913
  ;; TODO: Check bundled sources.
  (crate-source "ndk-sys" "0.6.0+11769913"
                "0wx8r6pji20if4xs04g73gxl98nmjrfc73z0v6w1ypv6a4qdlv7f"))

(define rust-net2-0.2.39
  (crate-source "net2" "0.2.39"
                "1b1lxvs192xsvqnszcz7dn4dw3fsvzxnc23qvq39scx26s068fxi"))

(define rust-new-debug-unreachable-1.0.6
  (crate-source "new_debug_unreachable" "1.0.6"
                "11phpf1mjxq6khk91yzcbd3ympm78m3ivl7xg6lg2c0lf66fy3k5"))

(define rust-newline-converter-0.3.0
  (crate-source "newline-converter" "0.3.0"
                "0zyw2hyjl89rj1zmp9n8fq69pbfp9zl1cbal73agxjxixjbv1dj7"))

(define rust-nix-0.26.4
  (crate-source "nix" "0.26.4"
                "06xgl4ybb8pvjrbmc3xggbgk3kbs1j0c4c0nzdfrmpbgrkrym2sr"))

(define rust-nix-0.27.1
  (crate-source "nix" "0.27.1"
                "0ly0kkmij5f0sqz35lx9czlbk6zpihb7yh1bsy4irzwfd2f4xc1f"))

(define rust-nix-0.28.0
  (crate-source "nix" "0.28.0"
                "1r0rylax4ycx3iqakwjvaa178jrrwiiwghcw95ndzy72zk25c8db"))

(define rust-nix-0.29.0
  (crate-source "nix" "0.29.0"
                "0ikvn7s9r2lrfdm3mx1h7nbfjvcc6s9vxdzw7j5xfkd2qdnp9qki"))

(define rust-nix-0.30.1
  (crate-source "nix" "0.30.1"
                "1dixahq9hk191g0c2ydc0h1ppxj0xw536y6rl63vlnp06lx3ylkl"))

(define rust-nom-3.2.1
  (crate-source "nom" "3.2.1"
                "0yr8fazcspgawl6s7wmx5llz61s68jl88cnrph18fa7xf06cbbh5"))

(define rust-nom-7.1.3
  (crate-source "nom" "7.1.3"
                "0jha9901wxam390jcf5pfa0qqfrgh8li787jx2ip0yk5b8y9hwyj"))

(define rust-nom-8.0.0
  (crate-source "nom" "8.0.0"
                "01cl5xng9d0gxf26h39m0l8lprgpa00fcc75ps1yzgbib1vn35yz"))

(define rust-nom-locate-5.0.0
  (crate-source "nom_locate" "5.0.0"
                "13a3d1f5gcqhjwchp1j2633wfk0wmpx2xdd2rd04fz42d4npwmqb"))

(define rust-noop-proc-macro-0.3.0
  (crate-source "noop_proc_macro" "0.3.0"
                "1j2v1c6ric4w9v12h34jghzmngcwmn0hll1ywly4h6lcm4rbnxh6"))

(define rust-normalize-line-endings-0.3.0
  (crate-source "normalize-line-endings" "0.3.0"
                "1gp52dfn2glz26a352zra8h04351icf0fkqzw1shkwrgh1vpz031"))

(define rust-normpath-1.5.0
  (crate-source "normpath" "1.5.0"
                "16z68q809749ky2vl72f3lqnhf3vjclvcc3y2z5v8m2nj0msn8xz"))

(define rust-notify-6.1.1
  (crate-source "notify" "6.1.1"
                "0bad98r0ilkhhq2jg3zs11zcqasgbvxia8224wpasm74n65vs1b2"))

(define rust-notify-8.2.0
  (crate-source "notify" "8.2.0"
                "1hrb83451vm5cpjw83nz5skgwjg5ara28zq8nxsqbzsif690fgad"))

(define rust-notify-types-2.0.0
  (crate-source "notify-types" "2.0.0"
                "0pcjm3wnvb7pvzw6mn89csv64ip0xhx857kr8jic5vddi6ljc22y"))

(define rust-notmuch-0.8.0
  (crate-source "notmuch" "0.8.0"
                "0i6xc7lv10m2sq6vlpjr5wxmlxihvd0v4f5if75r2kwz8ji12pg2"))

(define rust-ntapi-0.4.0
  (crate-source "ntapi" "0.4.0"
                "1z2a984vrmcg895gg34zjadfnmjic9nfachjyx0mj81b6rxxnldw"))

(define rust-ntapi-0.4.1
  (crate-source "ntapi" "0.4.1"
                "1r38zhbwdvkis2mzs6671cm1p6djgsl49i7bwxzrvhwicdf8k8z8"))

(define rust-nu-ansi-term-0.46.0
  (crate-source "nu-ansi-term" "0.46.0"
                "115sywxh53p190lyw97alm14nc004qj5jm5lvdj608z84rbida3p"))

(define rust-nu-ansi-term-0.50.1
  (crate-source "nu-ansi-term" "0.50.1"
                "16a3isvbxx8pa3lk71h3cq2fsx2d17zzq42j4mhpxy81gl2qx8nl"))

(define rust-nu-ansi-term-0.50.3
  (crate-source "nu-ansi-term" "0.50.3"
                "1ra088d885lbd21q1bxgpqdlk1zlndblmarn948jz2a40xsbjmvr"))

(define rust-num-0.4.3
  (crate-source "num" "0.4.3"
                "08yb2fc1psig7pkzaplm495yp7c30m4pykpkwmi5bxrgid705g9m"))

(define rust-num-bigint-0.4.6
  (crate-source "num-bigint" "0.4.6"
                "1f903zd33i6hkjpsgwhqwi2wffnvkxbn6rv4mkgcjcqi7xr4zr55"))

(define rust-num-bigint-dig-0.8.4
  (crate-source "num-bigint-dig" "0.8.4"
                "0lb12df24wgxxbspz4gw1sf1kdqwvpdcpwq4fdlwg4gj41c1k16w"))

(define rust-num-cmp-0.1.0
  (crate-source "num-cmp" "0.1.0"
                "1alavi36shn32b3cwbmkncj1wal3y3cwzkm21bxy5yil5hp5ncv3"))

(define rust-num-complex-0.4.6
  (crate-source "num-complex" "0.4.6"
                "15cla16mnw12xzf5g041nxbjjm9m85hdgadd5dl5d0b30w9qmy3k"))

(define rust-num-conv-0.1.0
  (crate-source "num-conv" "0.1.0"
                "1ndiyg82q73783jq18isi71a7mjh56wxrk52rlvyx0mi5z9ibmai"))

(define rust-num-cpus-1.15.0
  (crate-source "num_cpus" "1.15.0"
                "0fsrjy3arnbcl41vz0gppya8d7d24cpkjgfflr3v8pivl4nrxb0g"))

(define rust-num-cpus-1.16.0
  (crate-source "num_cpus" "1.16.0"
                "0hra6ihpnh06dvfvz9ipscys0xfqa9ca9hzp384d5m02ssvgqqa1"))

(define rust-num-cpus-1.17.0
  (crate-source "num_cpus" "1.17.0"
                "0fxjazlng4z8cgbmsvbzv411wrg7x3hyxdq8nxixgzjswyylppwi"))

(define rust-num-derive-0.3.3
  (crate-source "num-derive" "0.3.3"
                "0gbl94ckzqjdzy4j8b1p55mz01g6n1l9bckllqvaj0wfz7zm6sl7"))

(define rust-num-derive-0.4.2
  (crate-source "num-derive" "0.4.2"
                "00p2am9ma8jgd2v6xpsz621wc7wbn1yqi71g15gc3h67m7qmafgd"))

(define rust-num-enum-0.7.3
  (crate-source "num_enum" "0.7.3"
                "0yai0vafhy85mvhknzfqd7lm04hzaln7i5c599rhy8mj831kyqaf"))

(define rust-num-enum-0.7.5
  (crate-source "num_enum" "0.7.5"
                "0k25hagf3xfgmj4j1zmvja1d6844jrmpginxpd3vhmxd41z7l85i"))

(define rust-num-enum-derive-0.7.3
  (crate-source "num_enum_derive" "0.7.3"
                "0mksna1jj87ydh146gn6jcqkvvs920c3dgh0p4f3xk184kpl865g"))

(define rust-num-enum-derive-0.7.5
  (crate-source "num_enum_derive" "0.7.5"
                "1mx4dgza8b9g16baybc00gg06jn4cf17h45p0fr3qx5nw5fkccpz"))

(define rust-num-integer-0.1.46
  (crate-source "num-integer" "0.1.46"
                "13w5g54a9184cqlbsq80rnxw4jj4s0d8wv75jsq5r2lms8gncsbr"))

(define rust-num-iter-0.1.45
  (crate-source "num-iter" "0.1.45"
                "1gzm7vc5g9qsjjl3bqk9rz1h6raxhygbrcpbfl04swlh0i506a8l"))

(define rust-num-rational-0.4.2
  (crate-source "num-rational" "0.4.2"
                "093qndy02817vpgcqjnj139im3jl7vkq4h68kykdqqh577d18ggq"))

(define rust-num-threads-0.1.7
  (crate-source "num_threads" "0.1.7"
                "1ngajbmhrgyhzrlc4d5ga9ych1vrfcvfsiqz6zv0h2dpr2wrhwsw"))

(define rust-num-traits-0.2.18
  (crate-source "num-traits" "0.2.18"
                "0yjib8p2p9kzmaz48xwhs69w5dh1wipph9jgnillzd2x33jz03fs"))

(define rust-num-traits-0.2.19
  (crate-source "num-traits" "0.2.19"
                "0h984rhdkkqd4ny9cif7y2azl3xdfb7768hb9irhpsch4q3gq787"))

(define rust-nvapi-0.1.4
  (crate-source "nvapi" "0.1.4"
                "0n1ikdvwr38g1lc54anjv2l96qlfwrmfm3d4s4w2qbinv26dwqvw"))

(define rust-nvapi-sys-0.1.3
  ;; TODO: Check bundled sources.
  (crate-source "nvapi-sys" "0.1.3"
                "19vy3vy7hhik8gymi1gmsq6jqbgk3vnpcpygpxbfi7n6jf9rm7mj"))

(define rust-oauth-lib-2.0.0
  (crate-source "oauth-lib" "2.0.0"
                "0apraw0b90j8cb0x54a6k3wv3j6nqx2y1np3f4crspmwbxzkcjjx"))

(define rust-oauth2-5.0.0-rc.1
  (crate-source "oauth2" "5.0.0-rc.1"
                "1gw5cxhkkf65skz1ggkhfpgh8rn3f7nvww1gdl1jjbb07kd8blr3"))

(define rust-objc-0.2.7
  (crate-source "objc" "0.2.7"
                "1cbpf6kz8a244nn1qzl3xyhmp05gsg4n313c9m3567625d3innwi"))

(define rust-objc-exception-0.1.2
  (crate-source "objc_exception" "0.1.2"
                "191cmdmlypp6piw67y4m8y5swlxf5w0ss8n1lk5xd2l1ans0z5xd"))

(define rust-objc-foundation-0.1.1
  (crate-source "objc-foundation" "0.1.1"
                "1y9bwb3m5fdq7w7i4bnds067dhm4qxv4m1mbg9y61j9nkrjipp8s"))

(define rust-objc-id-0.1.1
  (crate-source "objc_id" "0.1.1"
                "0fq71hnp2sdblaighjc82yrac3adfmqzhpr11irhvdfp9gdlsbf9"))

(define rust-objc-rs-0.2.8
  (crate-source "objc-rs" "0.2.8"
                "1f0y59g793k9pz52gyw4wfnsypw32rwiz7ray5mi4995k83fg8b4"))

(define rust-objc-sys-0.3.5
  ;; TODO: Check bundled sources.
  (crate-source "objc-sys" "0.3.5"
                "0423gry7s3rmz8s3pzzm1zy5mdjif75g6dbzc2lf2z0c77fipffd"))

(define rust-objc2-0.5.2
  (crate-source "objc2" "0.5.2"
                "015qa2d3vh7c1j2736h5wjrznri7x5ic35vl916c22gzxva8b9s6"))

(define rust-objc2-0.6.2
  (crate-source "objc2" "0.6.2"
                "1g3qa1vxp6nlh4wllll921z299d3s1is31m1ccasd8pklxxka7sn"))

(define rust-objc2-0.6.3
  (crate-source "objc2" "0.6.3"
                "01ccrb558qav2rqrmk0clzqzdd6r1rmicqnf55xqam7cw2f5khmp"))

(define rust-objc2-app-kit-0.2.2
  (crate-source "objc2-app-kit" "0.2.2"
                "1zqyi5l1bm26j1bgmac9783ah36m5kcrxlqp5carglnpwgcrms74"))

(define rust-objc2-app-kit-0.3.2
  (crate-source "objc2-app-kit" "0.3.2"
                "132ijwni8lsi8phq7wnmialkxp46zx998fns3zq5np0ya1mr77nl"))

(define rust-objc2-audio-toolbox-0.3.2
  (crate-source "objc2-audio-toolbox" "0.3.2"
                "024vny0nxb93ihdk97q1zrbpism4i8xa7flsnycn678jj4d50j39"))

(define rust-objc2-cloud-kit-0.2.2
  (crate-source "objc2-cloud-kit" "0.2.2"
                "02dhjvmcq8c2bwj31jx423jygif1scs9f0lmlab0ayhw75b3ppbl"))

(define rust-objc2-contacts-0.2.2
  (crate-source "objc2-contacts" "0.2.2"
                "12a8m927xrrxa54xhqhqnkkl1a6l07pyrpnqfk9jz09kkh755zx5"))

(define rust-objc2-core-audio-0.3.2
  (crate-source "objc2-core-audio" "0.3.2"
                "1cn3d7cni2ngr18j14s4xfin3h4gqq3k2kshr3vzbgqdigmbrvp1"))

(define rust-objc2-core-audio-types-0.3.2
  (crate-source "objc2-core-audio-types" "0.3.2"
                "075xj0j67n59m6v7pa0d556l06imicg9kcj24siz832a4zng52as"))

(define rust-objc2-core-data-0.2.2
  (crate-source "objc2-core-data" "0.2.2"
                "1vvk8zjylfjjj04dzawydmqqz5ajvdkhf22cnb07ihbiw14vyzv1"))

(define rust-objc2-core-foundation-0.3.1
  (crate-source "objc2-core-foundation" "0.3.1"
                "0rn19d70mwxyv74kx7aqm5in6x320vavq9v0vrm81vbg9a4w440w"))

(define rust-objc2-core-foundation-0.3.2
  (crate-source "objc2-core-foundation" "0.3.2"
                "0dnmg7606n4zifyjw4ff554xvjmi256cs8fpgpdmr91gckc0s61a"))

(define rust-objc2-core-graphics-0.3.2
  (crate-source "objc2-core-graphics" "0.3.2"
                "01x8413pxq0m5rwidlaczni8v5cz9dc3xqzq8l9zlpl9cv8cj8p0"))

(define rust-objc2-core-image-0.2.2
  (crate-source "objc2-core-image" "0.2.2"
                "102csfb82zi2sbzliwsfd589ckz0gysf7y6434c9zj97lmihj9jm"))

(define rust-objc2-core-location-0.2.2
  (crate-source "objc2-core-location" "0.2.2"
                "10apgsrigqryvi4rcc0f6yfjflvrl83f4bi5hkr48ck89vizw300"))

(define rust-objc2-encode-4.0.3
  (crate-source "objc2-encode" "4.0.3"
                "1y7hjg4k828zhn4fjnbidrz3vzw4llk9ldy92drj47ydjc9yg4bq"))

(define rust-objc2-encode-4.1.0
  (crate-source "objc2-encode" "4.1.0"
                "0cqckp4cpf68mxyc2zgnazj8klv0z395nsgbafa61cjgsyyan9gg"))

(define rust-objc2-foundation-0.2.2
  (crate-source "objc2-foundation" "0.2.2"
                "1a6mi77jsig7950vmx9ydvsxaighzdiglk5d229k569pvajkirhf"))

(define rust-objc2-foundation-0.3.1
  (crate-source "objc2-foundation" "0.3.1"
                "0g5hl47dxzabs7wndcg6kz3q137v9hwfay1jd2da1q9gglj3224h"))

(define rust-objc2-foundation-0.3.2
  (crate-source "objc2-foundation" "0.3.2"
                "0wijkxzzvw2xkzssds3fj8279cbykz2rz9agxf6qh7y2agpsvq73"))

(define rust-objc2-io-kit-0.3.1
  (crate-source "objc2-io-kit" "0.3.1"
                "02iwv7pppxvna72xwd7y5q67hrnbn5v73xikc3c1rr90c56wdhbi"))

(define rust-objc2-io-surface-0.3.2
  (crate-source "objc2-io-surface" "0.3.2"
                "07fqx4fmwydf2arrc4xs4awv7zyzzxh60fyqdfmrpm9n148qh1qq"))

(define rust-objc2-link-presentation-0.2.2
  (crate-source "objc2-link-presentation" "0.2.2"
                "160k4qh00yrx57dabn3hzas4r98kmk9bc0qsy1jvwday3irax8d1"))

(define rust-objc2-metal-0.2.2
  (crate-source "objc2-metal" "0.2.2"
                "1mmdga66qpxrcfq3gxxhysfx3zg1hpx4z886liv3j0pnfq9bl36x"))

(define rust-objc2-quartz-core-0.2.2
  (crate-source "objc2-quartz-core" "0.2.2"
                "0ynw8819c36l11rim8n0yzk0fskbzrgaqayscyqi8swhzxxywaz4"))

(define rust-objc2-symbols-0.2.2
  (crate-source "objc2-symbols" "0.2.2"
                "1p04hjkxan18g2b7h9n2n8xxsvazapv2h6mfmmdk06zc7pz4ws0a"))

(define rust-objc2-ui-kit-0.2.2
  (crate-source "objc2-ui-kit" "0.2.2"
                "0vrb5r8z658l8c19bx78qks8c5hg956544yirf8npk90idwldfxq"))

(define rust-objc2-uniform-type-identifiers-0.2.2
  (crate-source "objc2-uniform-type-identifiers" "0.2.2"
                "1ziv4wkbxcaw015ypg0q49ycl7m14l3x56mpq2k1rznv92bmzyj4"))

(define rust-objc2-user-notifications-0.2.2
  (crate-source "objc2-user-notifications" "0.2.2"
                "1cscv2w3vxzaslz101ddv0z9ycrrs4ayikk4my4qd3im8bvcpkvn"))

(define rust-object-0.30.3
  (crate-source "object" "0.30.3"
                "0fdl7qjsz1j9kl3j7f4656fswzrqpyj2kgaizhknmjrx7mfjd1pa"))

(define rust-object-0.32.0
  (crate-source "object" "0.32.0"
                "1ghynapcbgzrmnbwmmxj129dbzvmh0hwx8bplmh8ra5f0yympb3p"))

(define rust-object-0.32.2
  (crate-source "object" "0.32.2"
                "0hc4cjwyngiy6k51hlzrlsxgv5z25vv7c2cp0ky1lckfic0259m6"))

(define rust-object-0.36.0
  (crate-source "object" "0.36.0"
                "0d3l415005kqs1xamy7va1vnyiyh55x7d78mpd531pzrr0gzwvap"))

(define rust-object-0.36.7
  (crate-source "object" "0.36.7"
                "11vv97djn9nc5n6w1gc6bd96d2qk2c8cg1kw5km9bsi3v4a8x532"))

(define rust-object-0.37.3
  (crate-source "object" "0.37.3"
                "1zikiy9xhk6lfx1dn2gn2pxbnfpmlkn0byd7ib1n720x0cgj0xpz"))

(define rust-oboe-0.6.1
  (crate-source "oboe" "0.6.1"
                "1yv7x06mwk61nsy3ckcmqwgg9q0n3j4y4zncz3sl6pcyskmipdp8"))

(define rust-oboe-sys-0.6.1
  ;; TODO: Check bundled sources.
  (crate-source "oboe-sys" "0.6.1"
                "17g7yb4kk6bakc4rhv1izfcqjgqhpkasgq6gf20nc79b9adb12vc"))

(define rust-octseq-0.5.2
  (crate-source "octseq" "0.5.2"
                "04pycbrcxlmhxqmrs4jgd0kqjk9pwjil6zr4fp2wwi4wgjikqv0j"))

(define rust-oid-registry-0.8.1
  (crate-source "oid-registry" "0.8.1"
                "1dxm6qkkkk4dq3ln1v83d80k8bvicm6mspsxrj3n06yy7pzhrx0j"))

(define rust-once-cell-1.18.0
  (crate-source "once_cell" "1.18.0"
                "0vapcd5ambwck95wyz3ymlim35jirgnqn9a0qmi19msymv95v2yx"))

(define rust-once-cell-1.19.0
  (crate-source "once_cell" "1.19.0"
                "14kvw7px5z96dk4dwdm1r9cqhhy2cyj1l5n5b29mynbb8yr15nrz"))

(define rust-once-cell-1.20.2
  (crate-source "once_cell" "1.20.2"
                "0xb7rw1aqr7pa4z3b00y7786gyf8awx2gca3md73afy76dzgwq8j"))

(define rust-once-cell-1.20.3
  (crate-source "once_cell" "1.20.3"
                "0bp6rgrsri1vfdcahsimk08zdiilv14ppgcnpbiw8hqyp2j64m4l"))

(define rust-once-cell-1.21.3
  (crate-source "once_cell" "1.21.3"
                "0b9x77lb9f1j6nqgf5aka4s2qj0nly176bpbrv6f9iakk5ff3xa2"))

(define rust-once-cell-polyfill-1.70.1
  (crate-source "once_cell_polyfill" "1.70.1"
                "1bg0w99srq8h4mkl68l1mza2n2f2hvrg0n8vfa3izjr5nism32d4"))

(define rust-once-cell-polyfill-1.70.2
  (crate-source "once_cell_polyfill" "1.70.2"
                "1zmla628f0sk3fhjdjqzgxhalr2xrfna958s632z65bjsfv8ljrq"))

(define rust-oorandom-11.1.4
  (crate-source "oorandom" "11.1.4"
                "1sg4j19r5302a6jpn0kgfkbjnslrqr3ynxv8x2h2ddaaw7kvn45l"))

(define rust-oorandom-11.1.5
  (crate-source "oorandom" "11.1.5"
                "07mlf13z453fq01qff38big1lh83j8l6aaglf63ksqzzqxc0yyfn"))

(define rust-open-1.7.1
  (crate-source "open" "1.7.1"
                "00828zcxdy3r38inz48jgnszgvqgi1a3bi2rrhij86mqsqq7msnw"))

(define rust-open-5.3.2
  (crate-source "open" "5.3.2"
                "15ggfx1p8rl7w4rr1n5qj1wxy1kk7757lsjpyc947a9fwri3aj72"))

(define rust-opener-0.8.3
  (crate-source "opener" "0.8.3"
                "0isfar4r3h25kf1z35mz8r1sdh8gilm3a51akp4007mr5ab2946b"))

(define rust-openssl-0.10.64
  (crate-source "openssl" "0.10.64"
                "07vb455yh08qh3n493ssw1qsa3zg3zfj438kk2180453hq94i84m"))

(define rust-openssl-0.10.66
  (crate-source "openssl" "0.10.66"
                "1hfr9ffx67j455aqrmyys3c8l65ngbqrl5qi3v3fi8vhddwg8acm"))

(define rust-openssl-0.10.68
  (crate-source "openssl" "0.10.68"
                "1xbiz2bmba2fibg70s462yk2fndp3f9vz11c7iw0ilh2y54bqx31"))

(define rust-openssl-0.10.72
  (crate-source "openssl" "0.10.72"
                "1np54pm6hw512rmfjv3kc54h8yvf51mdlm8a8cc33xx1b1yympzy"))

(define rust-openssl-0.10.74
  (crate-source "openssl" "0.10.74"
                "0m5nxchp0lmkqclgxs4ym3sg9w340s7sslhglghnj8j18pfi9b94"))

(define rust-openssl-0.10.75
  (crate-source "openssl" "0.10.75"
                "0a238gvrzjq0r62a7472i685hi5jjzgfj72kp1xd32ir46qqv0q8"))

(define rust-openssl-macros-0.1.1
  (crate-source "openssl-macros" "0.1.1"
                "173xxvfc63rr5ybwqwylsir0vq6xsj4kxiv4hmg4c3vscdmncj59"))

(define rust-openssl-probe-0.1.5
  (crate-source "openssl-probe" "0.1.5"
                "1kq18qm48rvkwgcggfkqq6pm948190czqc94d6bm2sir5hq1l0gz"))

(define rust-openssl-probe-0.1.6
  (crate-source "openssl-probe" "0.1.6"
                "0bl52x55laalqb707k009h8kfawliwp992rlsvkzy49n47p2fpnh"))

(define rust-openssl-probe-0.2.0
  (crate-source "openssl-probe" "0.2.0"
                "14bk0gw4sqw4fqywl2032d69k538r91a42mdf7byq2dvvarxjl4z"))

(define rust-openssl-src-300.3.2+3.3.2
  ;; TODO: Check bundled sources.
  (crate-source "openssl-src" "300.3.2+3.3.2"
                "0yv44gm6ds9h1lz21si2p6m4dvj8qps5h03fri4fdxsyjj6s24d2"))

(define rust-openssl-src-300.5.4+3.5.4
  ;; TODO: Check bundled sources.
  (crate-source "openssl-src" "300.5.4+3.5.4"
                "0wnbqw38pzp66axaw2wz5my8nhg8f4viw74avyqfknlm55wv61x5"))

(define rust-openssl-sys-0.9.102
  ;; TODO: Check bundled sources.
  (crate-source "openssl-sys" "0.9.102"
                "18jzni7xzdcqwf9r8kp6j46abrxqn82dvc2ylf9kij7varyn75y5"))

(define rust-openssl-sys-0.9.103
  ;; TODO: Check bundled sources.
  (crate-source "openssl-sys" "0.9.103"
                "1mi9r5vbgqqwfa2nqlh2m0r1v5abhzjigfbi7ja0mx0xx7p8v7kz"))

(define rust-openssl-sys-0.9.104
  ;; TODO: Check bundled sources.
  (crate-source "openssl-sys" "0.9.104"
                "0hf712xcxmycnlc09r8d446b3mwqchsbfrjv374fp7grrc3g7as5"))

(define rust-openssl-sys-0.9.107
  ;; TODO: Check bundled sources.
  (crate-source "openssl-sys" "0.9.107"
                "01yydv8yaagdnapvair8b6rggf225lwb854h99s9qx44rnd9g242"))

(define rust-openssl-sys-0.9.110
  ;; TODO: Check bundled sources.
  (crate-source "openssl-sys" "0.9.110"
                "1qnqrvgahzn9yfxjz3v4i29x860x9dc2c84bisgv089wp9sh17qa"))

(define rust-openssl-sys-0.9.111
  ;; TODO: Check bundled sources.
  (crate-source "openssl-sys" "0.9.111"
                "08f3mpsabivfi3fd0qv9231qidqy68lr8a4qi32y6xda43av5jl2"))

(define rust-option-ext-0.2.0
  (crate-source "option-ext" "0.2.0"
                "0zbf7cx8ib99frnlanpyikm1bx8qn8x602sw1n7bg6p9x94lyx04"))

(define rust-orbclient-0.3.48
  (crate-source "orbclient" "0.3.48"
                "0hzxjsvvsl5i9d3aqzc6kdcsch1i6flij5dkignhhkz2qb72c2xs"))

(define rust-orbclient-0.3.49
  (crate-source "orbclient" "0.3.49"
                "09xqjrpaxlvp1vs8l45d23vmqn934ijzh8acc3w3g54vw53d2yi4"))

(define rust-ordered-float-4.6.0
  (crate-source "ordered-float" "4.6.0"
                "0ldrcgilsiijd141vw51fbkziqmh5fpllil3ydhirjm67wdixdvv"))

(define rust-ordered-float-5.1.0
  (crate-source "ordered-float" "5.1.0"
                "0b8gs4syw1bb008ds3h6qb0sggbg5hchhvbqqd028mhsj337jivz"))

(define rust-ordered-multimap-0.4.3
  (crate-source "ordered-multimap" "0.4.3"
                "0jljv1257pfyf855jlwwas5mqkzk40b9lqfx40f73qbpf7ildmyc"))

(define rust-ordered-stream-0.2.0
  (crate-source "ordered-stream" "0.2.0"
                "0l0xxp697q7wiix1gnfn66xsss7fdhfivl2k7bvpjs4i3lgb18ls"))

(define rust-os-pipe-1.2.1
  (crate-source "os_pipe" "1.2.1"
                "10nrh0i507560rsiy4c79fajdmqgbr6dha2pbl9mncrlaq52pzaz"))

(define rust-os-str-bytes-6.6.1
  (crate-source "os_str_bytes" "6.6.1"
                "1885z1x4sm86v5p41ggrl49m58rbzzhd1kj72x46yy53p62msdg2"))

(define rust-ouroboros-0.15.6
  (crate-source "ouroboros" "0.15.6"
                "1nvjra9dana2g6kxv3397qrgpyw6lknzya6lzs1s1llbap8qndg1"))

(define rust-ouroboros-0.18.4
  (crate-source "ouroboros" "0.18.4"
                "0rsazk2hh2w626585scb7ylaf500y5insp3rnkbdwnm2jq4s4kwl"))

(define rust-ouroboros-0.18.5
  (crate-source "ouroboros" "0.18.5"
                "0ndy8hvp24gs7yxw9wj81hs5rb36wxmpw4i38ylrfjy4p46ha3qy"))

(define rust-ouroboros-macro-0.15.6
  (crate-source "ouroboros_macro" "0.15.6"
                "1dsn37vds4qpkzscmwaw17dv3m5m7a7j9qby8dsac19ks3622zaz"))

(define rust-ouroboros-macro-0.18.4
  (crate-source "ouroboros_macro" "0.18.4"
                "1gb5njxh9clp9krjc7kfbz17g5racjlld1bsjkjx13sjs7mdxc1r"))

(define rust-ouroboros-macro-0.18.5
  (crate-source "ouroboros_macro" "0.18.5"
                "1l343ss6hlh0abbwjk6zah6mdlyhh1v1imflv3v86c6lsfyjhw1w"))

(define rust-outref-0.5.2
  (crate-source "outref" "0.5.2"
                "03pzw9aj4qskqhh0fkagy2mkgfwgj5a1m67ajlba5hw80h68100s"))

(define rust-overload-0.1.1
  (crate-source "overload" "0.1.1"
                "0fdgbaqwknillagy1xq7xfgv60qdbk010diwl7s1p0qx7hb16n5i"))

(define rust-owned-ttf-parser-0.25.0
  (crate-source "owned_ttf_parser" "0.25.0"
                "1i5jhazmm87nf8wbqm76vjvncmcv28ghpqm4170q2aivpydp3v12"))

(define rust-owned-ttf-parser-0.25.1
  (crate-source "owned_ttf_parser" "0.25.1"
                "0fsqzcbc4sq8qhkmc3rgcfg1xg389nmhlxvmvi6h38dca680x0in"))

(define rust-owo-colors-3.5.0
  (crate-source "owo-colors" "3.5.0"
                "0vyvry6ba1xmpd45hpi6savd8mbx09jpmvnnwkf6z62pk6s4zc61"))

(define rust-owo-colors-4.2.1
  (crate-source "owo-colors" "4.2.1"
                "1v0cvfas9v8fkk4w8jk3m51igj57sjp1cry3aqv7z88z40bm7696"))

(define rust-p256-0.13.2
  (crate-source "p256" "0.13.2"
                "0jyd3c3k239ybs59ixpnl7dqkmm072fr1js8kh7ldx58bzc3m1n9"))

(define rust-p384-0.13.0
  (crate-source "p384" "0.13.0"
                "02cjlxdvxwvhmnckqnydqpvrwhf5raj67q300d66m7y6pi8nyy3h"))

(define rust-palette-0.7.6
  (crate-source "palette" "0.7.6"
                "1rmn02mv6cb112504qyg7pyfa83c08hxpk5sw7jc5v659hc73gsc"))

(define rust-palette-derive-0.7.6
  (crate-source "palette_derive" "0.7.6"
                "0c0xhpk1nqyq4jr2m8xnka7w47vqzc7m2vq9ih8wxyjv02phs0zm"))

(define rust-pango-0.18.3
  (crate-source "pango" "0.18.3"
                "1r5ygq7036sv7w32kp8yxr6vgggd54iaavh3yckanmq4xg0px8kw"))

(define rust-pango-0.21.3
  (crate-source "pango" "0.21.3"
                "0bj08nhkibkfynkhivyxsn89bjfsnavkjzjz93rfkhhqirkplyz3"))

(define rust-pango-sys-0.18.0
  ;; TODO: Check bundled sources.
  (crate-source "pango-sys" "0.18.0"
                "1iaxalcaaj59cl9n10svh4g50v8jrc1a36kd7n9yahx8j7ikfrs3"))

(define rust-pango-sys-0.21.2
  ;; TODO: Check bundled sources.
  (crate-source "pango-sys" "0.21.2"
                "154w00ccb4gpb2pwqasm4dy66xzbxc7dm4h05agvlgx43prdmxgl"))

(define rust-parking-2.2.0
  (crate-source "parking" "2.2.0"
                "1blwbkq6im1hfxp5wlbr475mw98rsyc0bbr2d5n16m38z253p0dv"))

(define rust-parking-2.2.1
  (crate-source "parking" "2.2.1"
                "1fnfgmzkfpjd69v4j9x737b1k8pnn054bvzcn5dm3pkgq595d3gk"))

(define rust-parking-lot-0.11.2
  (crate-source "parking_lot" "0.11.2"
                "16gzf41bxmm10x82bla8d6wfppy9ym3fxsmdjyvn61m66s0bf5vx"))

(define rust-parking-lot-0.12.1
  (crate-source "parking_lot" "0.12.1"
                "13r2xk7mnxfc5g0g6dkdxqdqad99j7s7z8zhzz4npw5r0g0v4hip"))

(define rust-parking-lot-0.12.3
  (crate-source "parking_lot" "0.12.3"
                "09ws9g6245iiq8z975h8ycf818a66q3c6zv4b5h8skpm7hc1igzi"))

(define rust-parking-lot-0.12.4
  (crate-source "parking_lot" "0.12.4"
                "04sab1c7304jg8k0d5b2pxbj1fvgzcf69l3n2mfpkdb96vs8pmbh"))

(define rust-parking-lot-0.12.5
  (crate-source "parking_lot" "0.12.5"
                "06jsqh9aqmc94j2rlm8gpccilqm6bskbd67zf6ypfc0f4m9p91ck"))

(define rust-parking-lot-core-0.8.6
  (crate-source "parking_lot_core" "0.8.6"
                "1p2nfcbr0b9lm9rglgm28k6mwyjwgm4knipsmqbgqaxdy3kcz8k0"))

(define rust-parking-lot-core-0.9.10
  (crate-source "parking_lot_core" "0.9.10"
                "1y3cf9ld9ijf7i4igwzffcn0xl16dxyn4c5bwgjck1dkgabiyh0y"))

(define rust-parking-lot-core-0.9.11
  (crate-source "parking_lot_core" "0.9.11"
                "19g4d6m5k4ggacinqprnn8xvdaszc3y5smsmbz1adcdmaqm8v0xw"))

(define rust-parking-lot-core-0.9.12
  (crate-source "parking_lot_core" "0.9.12"
                "1hb4rggy70fwa1w9nb0svbyflzdc69h047482v2z3sx2hmcnh896"))

(define rust-parking-lot-core-0.9.8
  (crate-source "parking_lot_core" "0.9.8"
                "0ixlak319bpzldq20yvyfqk0y1vi736zxbw101jvzjp7by30rw4k"))

(define rust-parking-lot-core-0.9.9
  (crate-source "parking_lot_core" "0.9.9"
                "13h0imw1aq86wj28gxkblhkzx6z1gk8q18n0v76qmmj6cliajhjc"))

(define rust-paste-1.0.14
  (crate-source "paste" "1.0.14"
                "0k7d54zz8zrz0623l3xhvws61z5q2wd3hkwim6gylk8212placfy"))

(define rust-paste-1.0.15
  (crate-source "paste" "1.0.15"
                "02pxffpdqkapy292harq6asfjvadgp1s005fip9ljfsn9fvxgh2p"))

(define rust-pastey-0.1.1
  (crate-source "pastey" "0.1.1"
                "1v389jkifv757903flrrps67dvc6q6giwlyx3xi33hcfjmgjxyrm"))

(define rust-pathdiff-0.2.3
  (crate-source "pathdiff" "0.2.3"
                "1lrqp4ip05df8dzldq6gb2c1sq2gs54gly8lcnv3rhav1qhwx56z"))

(define rust-pathfinder-geometry-0.5.1
  (crate-source "pathfinder_geometry" "0.5.1"
                "1lssir0s1cmrpzzrk49jm31nkssh2j715gryww6700x79rxpwyqb"))

(define rust-pathfinder-simd-0.5.5
  (crate-source "pathfinder_simd" "0.5.5"
                "0mxdnmcagfv0h18ns8kdmirgk9c1993ih6c4z7zkmgsm0fb2g45z"))

(define rust-pcap-2.2.0
  (crate-source "pcap" "2.2.0"
                "0lh648phy9qsz14spz65n67g8lxji68ssph91jy2zxk5c642b4a9"))

(define rust-pcap-2.3.0
  (crate-source "pcap" "2.3.0"
                "02k6dnyjjpxqsnjsjcagmfdvp9pv4cac6k39ccsyrnc09b1spkc3"))

(define rust-peeking-take-while-0.1.2
  (crate-source "peeking_take_while" "0.1.2"
                "16bhqr6rdyrp12zv381cxaaqqd0pwysvm1q8h2ygihvypvfprc8r"))

(define rust-pem-3.0.6
  (crate-source "pem" "3.0.6"
                "1glia9vv51wx79cysqxgdha6g1bwbbr20bfhijlk2nxw4qycac0x"))

(define rust-pem-rfc7468-0.7.0
  (crate-source "pem-rfc7468" "0.7.0"
                "04l4852scl4zdva31c1z6jafbak0ni5pi0j38ml108zwzjdrrcw8"))

(define rust-percent-encoding-2.3.1
  (crate-source "percent-encoding" "2.3.1"
                "0gi8wgx0dcy8rnv1kywdv98lwcx67hz0a0zwpib5v2i08r88y573"))

(define rust-percent-encoding-2.3.2
  (crate-source "percent-encoding" "2.3.2"
                "083jv1ai930azvawz2khv7w73xh8mnylk7i578cifndjn5y64kwv"))

(define rust-persy-1.6.0
  (crate-source "persy" "1.6.0"
                "0hdi9v36j0yd7rpys2yki51061iwq2wawgs72wahp470b40pjsy2"))

(define rust-persy-1.7.1
  (crate-source "persy" "1.7.1"
                "19f523gwnh964rd7h0a8p4aqwz3i5jxig7frg2cxvp3x5g86mkhv"))

(define rust-pest-2.7.2
  (crate-source "pest" "2.7.2"
                "0npylniylqnrc3l9sfipal4zlp2yh2kr987ijfd78gx1cm1lmjqs"))

(define rust-pest-2.8.0
  (crate-source "pest" "2.8.0"
                "1dp741bxqiracvvwl66mfvlr29byvvph28n4c6ip136m652vg38r"))

(define rust-pest-2.8.3
  (crate-source "pest" "2.8.3"
                "1x3xc1s5vhwswmmr51i60kfbcnp1zgdblsxbqd8dxvs0l0hpb7lq"))

(define rust-pest-2.8.5
  (crate-source "pest" "2.8.5"
                "1xvm5gz0b2r629rilfpnn9fl5riii8sqs5ak9dqflr5445fb17ic"))

(define rust-pest-derive-2.7.2
  (crate-source "pest_derive" "2.7.2"
                "0lx88fs4hz0k8i2a9flv04h07yza032khn553l01bj2a1m4h0vb6"))

(define rust-pest-derive-2.8.0
  (crate-source "pest_derive" "2.8.0"
                "1icp5i01mgpbgwbkrcy4d0ykbxmns4wyz8j1jg6dr1wysz7xj9fp"))

(define rust-pest-derive-2.8.3
  (crate-source "pest_derive" "2.8.3"
                "1pp2g39k2vjdyzr89k8zx5y7pp3np4iv635jpyxzmfhd0fisjz8q"))

(define rust-pest-derive-2.8.5
  (crate-source "pest_derive" "2.8.5"
                "1v9ln8pklrinkd8vhlrm29bpwb0s9dv7416qg7x43i19sg7dpyb8"))

(define rust-pest-generator-2.7.2
  (crate-source "pest_generator" "2.7.2"
                "0a891sqpwgafiwxw1bjkibx26m2k1mvzhskqq63xmnshdx203jk8"))

(define rust-pest-generator-2.8.0
  (crate-source "pest_generator" "2.8.0"
                "0hgqngsxfr8y5p47bgjvd038j55ix1x4dpzr6amndaz8ddr02zfv"))

(define rust-pest-generator-2.8.3
  (crate-source "pest_generator" "2.8.3"
                "0hr80m5xzzrhzjvnmbawk72cxvn0ssc5j216gblynmspizch3d29"))

(define rust-pest-generator-2.8.5
  (crate-source "pest_generator" "2.8.5"
                "1rcfp9inzxm7alkfvj9zjazhxnqah672swcg7ks493x7a586vf9v"))

(define rust-pest-meta-2.7.2
  (crate-source "pest_meta" "2.7.2"
                "0j0a882n1rxf3ljhsydglnhiddbrr58x1ngn1df49l3lmwq0mbsn"))

(define rust-pest-meta-2.8.0
  (crate-source "pest_meta" "2.8.0"
                "182w5fyiqm7zbn0p8313xc5wc73rnn59ycm5zk8hcja9f0j877vz"))

(define rust-pest-meta-2.8.3
  (crate-source "pest_meta" "2.8.3"
                "0nh6w1mv8hx0p1jli8s12j2w62ia2apsbyl69nf07yg9zqn7mwkj"))

(define rust-pest-meta-2.8.5
  (crate-source "pest_meta" "2.8.5"
                "0r93xxn1hvgbhfnlj3ibpllsp14z1gcwz46lrxq1fqp8nnsi68b0"))

(define rust-petgraph-0.6.5
  (crate-source "petgraph" "0.6.5"
                "1ns7mbxidnn2pqahbbjccxkrqkrll2i5rbxx43ns6rh6fn3cridl"))

(define rust-petgraph-0.8.2
  (crate-source "petgraph" "0.8.2"
                "1jm7p01qvh3kxlrd437cqx6dryrcjd6lw9ky8cz562r2hnkg7b2l"))

(define rust-petgraph-0.8.3
  (crate-source "petgraph" "0.8.3"
                "0mblnaqbx1y20h5y7pz6y11hk9jjk6k87lsmn7jxaq3hm67ba0c7"))

(define rust-pgp-0.10.2
  (crate-source "pgp" "0.10.2"
                "0cp1pdiv0v1s2llk30xk8zc9r84hpsndlggycdbviadzhphgiq97"))

(define rust-pgp-lib-1.0.0
  (crate-source "pgp-lib" "1.0.0"
                "1mzywmwc73xjxsbqqj2vpqjsmz563f3pg78hswqsmnj3p16adcb2"))

(define rust-phf-0.11.2
  (crate-source "phf" "0.11.2"
                "1p03rsw66l7naqhpgr1a34r9yzi1gv9jh16g3fsk6wrwyfwdiqmd"))

(define rust-phf-0.11.3
  (crate-source "phf" "0.11.3"
                "0y6hxp1d48rx2434wgi5g8j1pr8s5jja29ha2b65435fh057imhz"))

(define rust-phf-0.12.1
  (crate-source "phf" "0.12.1"
                "1dz85g1wshfca83mrq3va9rm9n8qcdjlpv1i3908y5zc9j4p6cli"))

(define rust-phf-0.13.1
  (crate-source "phf" "0.13.1"
                "1pzswx5gdglgjgp4azyzwyr4gh031r0kcnpqq6jblga72z3jsmn1"))

(define rust-phf-codegen-0.11.2
  (crate-source "phf_codegen" "0.11.2"
                "0nia6h4qfwaypvfch3pnq1nd2qj64dif4a6kai3b7rjrsf49dlz8"))

(define rust-phf-codegen-0.11.3
  (crate-source "phf_codegen" "0.11.3"
                "0si1n6zr93kzjs3wah04ikw8z6npsr39jw4dam8yi9czg2609y5f"))

(define rust-phf-codegen-0.13.1
  (crate-source "phf_codegen" "0.13.1"
                "1qfnsl2hiny0yg4lwn888xla5iwccszgxnx8dhbwl6s2h2fpzaj9"))

(define rust-phf-generator-0.11.2
  (crate-source "phf_generator" "0.11.2"
                "1c14pjyxbcpwkdgw109f7581cc5fa3fnkzdq1ikvx7mdq9jcrr28"))

(define rust-phf-generator-0.11.3
  (crate-source "phf_generator" "0.11.3"
                "0gc4np7s91ynrgw73s2i7iakhb4lzdv1gcyx7yhlc0n214a2701w"))

(define rust-phf-generator-0.13.1
  (crate-source "phf_generator" "0.13.1"
                "0dwpp11l41dy9mag4phkyyvhpf66lwbp79q3ik44wmhyfqxcwnhk"))

(define rust-phf-macros-0.11.2
  (crate-source "phf_macros" "0.11.2"
                "0js61lc0bhzzrbd9vhpcqp11vvwckdkz3g7k95z5h1k651p68i1l"))

(define rust-phf-macros-0.11.3
  (crate-source "phf_macros" "0.11.3"
                "05kjfbyb439344rhmlzzw0f9bwk9fp95mmw56zs7yfn1552c0jpq"))

(define rust-phf-macros-0.13.1
  (crate-source "phf_macros" "0.13.1"
                "1vv9h8pr7xh18sigpvq1hxc8q9nmjmv6gdpqsp65krxiahmh6bw1"))

(define rust-phf-shared-0.11.2
  (crate-source "phf_shared" "0.11.2"
                "0azphb0a330ypqx3qvyffal5saqnks0xvl8rj73jlk3qxxgbkz4h"))

(define rust-phf-shared-0.11.3
  (crate-source "phf_shared" "0.11.3"
                "1rallyvh28jqd9i916gk5gk2igdmzlgvv5q0l3xbf3m6y8pbrsk7"))

(define rust-phf-shared-0.12.1
  (crate-source "phf_shared" "0.12.1"
                "10cr16wpmbjxd7w6k98sxw9yw3zxnzscybl9jzyq3digi045a006"))

(define rust-phf-shared-0.13.1
  (crate-source "phf_shared" "0.13.1"
                "0rpjchnswm0x5l4mz9xqfpw0j4w68sjvyqrdrv13h7lqqmmyyzz5"))

(define rust-pico-args-0.5.0
  (crate-source "pico-args" "0.5.0"
                "05d30pvxd6zlnkg2i3ilr5a70v3f3z2in18m67z25vinmykngqav"))

(define rust-pimalaya-tui-0.2.2
  (crate-source "pimalaya-tui" "0.2.2"
                "072lkdnhsw04i9vbfrmry7mr6hhxpm9hh9rw4kykvk3azp6vhxhc"))

(define rust-pin-project-1.1.10
  (crate-source "pin-project" "1.1.10"
                "12kadbnfm1f43cyadw9gsbyln1cy7vj764wz5c8wxaiza3filzv7"))

(define rust-pin-project-1.1.4
  (crate-source "pin-project" "1.1.4"
                "1q07737j774zxffdrypncdsrc5zx7dffw6l4dzanni9c8jhc80h3"))

(define rust-pin-project-1.1.5
  (crate-source "pin-project" "1.1.5"
                "1cxl146x0q7lawp0m1826wsgj8mmmfs6ja8q7m6f7ff5j6vl7gxn"))

(define rust-pin-project-1.1.7
  (crate-source "pin-project" "1.1.7"
                "15cvflrzsgp1zbl5gv37al2r62nl8lc37xkfwf70ql3fji7gcmxy"))

(define rust-pin-project-1.1.8
  (crate-source "pin-project" "1.1.8"
                "05jr3xfy1spgmz3q19l4mmvv46vgvkvsgphamifx7x45swxcabhy"))

(define rust-pin-project-internal-1.1.10
  (crate-source "pin-project-internal" "1.1.10"
                "0qgqzfl0f4lzaz7yl5llhbg97g68r15kljzihaw9wm64z17qx4bf"))

(define rust-pin-project-internal-1.1.4
  (crate-source "pin-project-internal" "1.1.4"
                "141nmib9lqbisrf8rz6fayy6l4fiw2r547h6af6npiy9c0mh8v16"))

(define rust-pin-project-internal-1.1.5
  (crate-source "pin-project-internal" "1.1.5"
                "0r9r4ivwiyqf45sv6b30l1dx282lxaax2f6gl84jwa3q590s8f1g"))

(define rust-pin-project-internal-1.1.7
  (crate-source "pin-project-internal" "1.1.7"
                "133mxf5vmvnvw4idw2y2lb5bxsza2xlyfl6psjy7mz3l12nmy3rw"))

(define rust-pin-project-internal-1.1.8
  (crate-source "pin-project-internal" "1.1.8"
                "1yzfhf6l27nhzv7r5hfrwj2g0x7xmfhgil19fj9am4srqp06csnm"))

(define rust-pin-project-lite-0.2.12
  (crate-source "pin-project-lite" "0.2.12"
                "018a7yg2zjcfby4832yw7s9091mgy6syfm369fjpfykjy45ipk0j"))

(define rust-pin-project-lite-0.2.13
  (crate-source "pin-project-lite" "0.2.13"
                "0n0bwr5qxlf0mhn2xkl36sy55118s9qmvx2yl5f3ixkb007lbywa"))

(define rust-pin-project-lite-0.2.14
  (crate-source "pin-project-lite" "0.2.14"
                "00nx3f04agwjlsmd3mc5rx5haibj2v8q9b52b0kwn63wcv4nz9mx"))

(define rust-pin-project-lite-0.2.15
  (crate-source "pin-project-lite" "0.2.15"
                "1zz4xif3iknfrpmvqmh0pcc9mx4cxm28jywqydir3pimcla1wnli"))

(define rust-pin-project-lite-0.2.16
  (crate-source "pin-project-lite" "0.2.16"
                "16wzc7z7dfkf9bmjin22f5282783f6mdksnr0nv0j5ym5f9gyg1v"))

(define rust-pin-utils-0.1.0
  (crate-source "pin-utils" "0.1.0"
                "117ir7vslsl2z1a7qzhws4pd01cg2d3338c47swjyvqv2n60v1wb"))

(define rust-piper-0.2.4
  (crate-source "piper" "0.2.4"
                "0rn0mjjm0cwagdkay77wgmz3sqf8fqmv9d9czm79mvr2yj8c9j4n"))

(define rust-pkcs1-0.7.5
  (crate-source "pkcs1" "0.7.5"
                "0zz4mil3nchnxljdfs2k5ab1cjqn7kq5lqp62n9qfix01zqvkzy8"))

(define rust-pkcs8-0.10.2
  (crate-source "pkcs8" "0.10.2"
                "1dx7w21gvn07azszgqd3ryjhyphsrjrmq5mmz1fbxkj5g0vv4l7r"))

(define rust-pkg-config-0.3.26
  (crate-source "pkg-config" "0.3.26"
                "0q2i61dhqvawc51zfzl3jich57w0cjgfa894hn6ings7ffgsbjba"))

(define rust-pkg-config-0.3.29
  (crate-source "pkg-config" "0.3.29"
                "1jy6158v1316khkpmq2sjj1vgbnbnw51wffx7p0k0l9h9vlys019"))

(define rust-pkg-config-0.3.30
  (crate-source "pkg-config" "0.3.30"
                "1v07557dj1sa0aly9c90wsygc0i8xv5vnmyv0g94lpkvj8qb4cfj"))

(define rust-pkg-config-0.3.31
  (crate-source "pkg-config" "0.3.31"
                "1wk6yp2phl91795ia0lwkr3wl4a9xkrympvhqq8cxk4d75hwhglm"))

(define rust-pkg-config-0.3.32
  (crate-source "pkg-config" "0.3.32"
                "0k4h3gnzs94sjb2ix6jyksacs52cf1fanpwsmlhjnwrdnp8dppby"))

(define rust-plain-0.2.3
  (crate-source "plain" "0.2.3"
                "19n1xbxb4wa7w891268bzf6cbwq4qvdb86bik1z129qb0xnnnndl"))

(define rust-platform-dirs-0.3.0
  (crate-source "platform-dirs" "0.3.0"
                "0f4ak4yfwy207anjncnnbdza5wb3l99lhimmg1gri4m6q51x1271"))

(define rust-plist-1.8.0
  (crate-source "plist" "1.8.0"
                "01qyv51ljbvhjbg8mva5c802b3dzrr95y6nd23wjh52xbjhvw3kl"))

(define rust-plotters-0.3.6
  (crate-source "plotters" "0.3.6"
                "1wqwn2fdavsk6lvhcykr4nk973nflijzwi1yb8ch4h28p366wnx1"))

(define rust-plotters-0.3.7
  (crate-source "plotters" "0.3.7"
                "0ixpy9svpmr2rkzkxvvdpysjjky4gw104d73n7pi2jbs7m06zsss"))

(define rust-plotters-backend-0.3.6
  (crate-source "plotters-backend" "0.3.6"
                "1dxdxdy31fkaqwp8bq7crbkn7kw7zs6i4mhwx80fjjk3qrifqk21"))

(define rust-plotters-backend-0.3.7
  (crate-source "plotters-backend" "0.3.7"
                "0ahpliim4hrrf7d4ispc2hwr7rzkn6d6nf7lyyrid2lm28yf2hnz"))

(define rust-plotters-iced-0.11.0
  (crate-source "plotters-iced" "0.11.0"
                "16jqy24a7jmn8hmjwh8cz3nqa2ya65cgnqw24jzzrj7bl7fy450a"))

(define rust-plotters-svg-0.3.6
  (crate-source "plotters-svg" "0.3.6"
                "01g74kchmz4lyyv5wbzmfj2i7wi9db9bv122p08f1hyrly30dcw1"))

(define rust-plotters-svg-0.3.7
  (crate-source "plotters-svg" "0.3.7"
                "0w56sxaa2crpasa1zj0bhxzihlapqfkncggavyngg0w86anf5fji"))

(define rust-png-0.17.16
  (crate-source "png" "0.17.16"
                "09kmkms9fmkbkarw0lnf0scqvjwwg3r7riddag0i3q39r0pil5c2"))

(define rust-png-0.18.0
  (crate-source "png" "0.18.0"
                "187jf0m873qn5biix8z7gjdsyf8r6vj3yr495pa0jja6i39wxflp"))

(define rust-polling-2.7.0
  (crate-source "polling" "2.7.0"
                "1aklz7gfkscmi9p21ic5hg7k0l0awgd17hrm17svyinxd9mcdqab"))

(define rust-polling-3.10.0
  (crate-source "polling" "3.10.0"
                "0afqlnm45081k06sngc052k9vmh33j2rqrmjgi7q1zjhcca1kgdm"))

(define rust-polling-3.11.0
  (crate-source "polling" "3.11.0"
                "0622qfbxi3gb0ly2c99n3xawp878fkrd1sl83hjdhisx11cly3jx"))

(define rust-polling-3.3.1
  (crate-source "polling" "3.3.1"
                "17hwk4g8qbdsyr0kqjddhw0l2v64pxhakkdlaqbc24xk99iglqyg"))

(define rust-polling-3.7.4
  (crate-source "polling" "3.7.4"
                "0bs4nhwfwsvlzlhah2gbhj3aa9ynvchv2g350wapswh26a65c156"))

(define rust-polling-3.8.0
  (crate-source "polling" "3.8.0"
                "0l7xlnzans2hc4zjxgplfp7gs16h9z3vdklnlpb2s05dj51nhfmm"))

(define rust-pollster-0.3.0
  (crate-source "pollster" "0.3.0"
                "1wn73ljx1pcb4p69jyiz206idj7nkfqknfvdhp64yaphhm3nys12"))

(define rust-pollster-0.4.0
  (crate-source "pollster" "0.4.0"
                "1qqcn0h2bvmgm9rlhfrdk7lfaiw1ad86g9500bhx1rj1s0c9yfig"))

(define rust-portable-atomic-1.11.0
  (crate-source "portable-atomic" "1.11.0"
                "0glb2wngflvfmg789qbf6dbnwcf6ai212fs7n0lf1c66rd49n3im"))

(define rust-portable-atomic-1.11.1
  (crate-source "portable-atomic" "1.11.1"
                "10s4cx9y3jvw0idip09ar52s2kymq8rq9a668f793shn1ar6fhpq"))

(define rust-portable-atomic-1.13.0
  (crate-source "portable-atomic" "1.13.0"
                "0l79rf3pzlxmmrylr1c4k61qn8hzs6hzz69yk738pdcvsvj7d5zq"))

(define rust-portable-atomic-util-0.2.4
  (crate-source "portable-atomic-util" "0.2.4"
                "01rmx1li07ixsx3sqg2bxqrkzk7b5n8pibwwf2589ms0s3cg18nq"))

(define rust-potential-utf-0.1.2
  (crate-source "potential_utf" "0.1.2"
                "11dm6k3krx3drbvhgjw8z508giiv0m09wzl6ghza37176w4c79z5"))

(define rust-potential-utf-0.1.3
  (crate-source "potential_utf" "0.1.3"
                "12mhwvhpvvim6xqp6ifgkh1sniv9j2cmid6axn10fnjvpsnikpw4"))

(define rust-potential-utf-0.1.4
  (crate-source "potential_utf" "0.1.4"
                "0xxg0pkfpq299wvwln409z4fk80rbv55phh3f1jhjajy5x1ljfdp"))

(define rust-powerfmt-0.2.0
  (crate-source "powerfmt" "0.2.0"
                "14ckj2xdpkhv3h6l5sdmb9f1d57z8hbfpdldjc2vl5givq2y77j3"))

(define rust-ppv-lite86-0.2.17
  (crate-source "ppv-lite86" "0.2.17"
                "1pp6g52aw970adv3x2310n7glqnji96z0a9wiamzw89ibf0ayh2v"))

(define rust-ppv-lite86-0.2.20
  (crate-source "ppv-lite86" "0.2.20"
                "017ax9ssdnpww7nrl1hvqh2lzncpv04nnsibmnw9nxjnaqlpp5bp"))

(define rust-ppv-lite86-0.2.21
  (crate-source "ppv-lite86" "0.2.21"
                "1abxx6qz5qnd43br1dd9b2savpihzjza8gb4fbzdql1gxp2f7sl5"))

(define rust-predicates-3.1.2
  (crate-source "predicates" "3.1.2"
                "15rcyjax4ykflw5425wsyzcfkgl08c9zsa8sdlsrmhj0fv68d43y"))

(define rust-predicates-3.1.3
  (crate-source "predicates" "3.1.3"
                "0wrm57acvagx0xmh5xffx5xspsr2kbggm698x0vks132fpjrxld5"))

(define rust-predicates-core-1.0.8
  (crate-source "predicates-core" "1.0.8"
                "0c8rl6d7qkcl773fw539h61fhlgdg7v9yswwb536hpg7x2z7g0df"))

(define rust-predicates-core-1.0.9
  (crate-source "predicates-core" "1.0.9"
                "1yjz144yn3imq2r4mh7k9h0r8wv4yyjjj57bs0zwkscz24mlczkj"))

(define rust-predicates-tree-1.0.11
  (crate-source "predicates-tree" "1.0.11"
                "04zv0i9pjfrldnvyxf4y07n243nvk3n4g03w2k6nccgdjp8l1ds1"))

(define rust-predicates-tree-1.0.12
  (crate-source "predicates-tree" "1.0.12"
                "0p223d9y02ywwxs3yl68kziswz4da4vabz67jfhp7yqx71njvpbj"))

(define rust-presser-0.3.1
  (crate-source "presser" "0.3.1"
                "1ykvqx861sjmhkdh540aafqba7i7li7gqgwrcczy6v56i9m8xkz8"))

(define rust-pretty-assertions-1.4.1
  (crate-source "pretty_assertions" "1.4.1"
                "0v8iq35ca4rw3rza5is3wjxwsf88303ivys07anc5yviybi31q9s"))

(define rust-prettyplease-0.2.29
  (crate-source "prettyplease" "0.2.29"
                "1b2zi42b5x3yckpin4ina1gr3n2m9zvvjmwdlhzzwz8zdv8cw939"))

(define rust-prettyplease-0.2.32
  (crate-source "prettyplease" "0.2.32"
                "1xmdmwhsvqc8l5ns029vzjida4k3lp5ynin0xra43qsiki0wakk6"))

(define rust-prettyplease-0.2.36
  (crate-source "prettyplease" "0.2.36"
                "1lj4rrjqbywjm82i3g7ilbskzdqvgqid8k7zdy0vjlj4lk6xy97z"))

(define rust-prettyplease-0.2.37
  (crate-source "prettyplease" "0.2.37"
                "0azn11i1kh0byabhsgab6kqs74zyrg69xkirzgqyhz6xmjnsi727"))

(define rust-primeorder-0.13.6
  (crate-source "primeorder" "0.13.6"
                "1rp16710mxksagcjnxqjjq9r9wf5vf72fs8wxffnvhb6i6hiqgim"))

(define rust-proc-macro-crate-1.3.1
  (crate-source "proc-macro-crate" "1.3.1"
                "069r1k56bvgk0f58dm5swlssfcp79im230affwk6d9ck20g04k3z"))

(define rust-proc-macro-crate-2.0.0
  (crate-source "proc-macro-crate" "2.0.0"
                "1s23imns07vmacn2xjd5hv2h6rr94iqq3fd2frwa6i4h2nk6d0vy"))

(define rust-proc-macro-crate-2.0.2
  (crate-source "proc-macro-crate" "2.0.2"
                "092x5acqnic14cw6vacqap5kgknq3jn4c6jij9zi6j85839jc3xh"))

(define rust-proc-macro-crate-3.2.0
  (crate-source "proc-macro-crate" "3.2.0"
                "0yzsqnavb3lmrcsmbrdjfrky9vcbl46v59xi9avn0796rb3likwf"))

(define rust-proc-macro-crate-3.4.0
  (crate-source "proc-macro-crate" "3.4.0"
                "10v9qi51n4phn1lrj5r94kjq7yhci9jrkqnn6wpan05yjsgb3711"))

(define rust-proc-macro-error-1.0.4
  (crate-source "proc-macro-error" "1.0.4"
                "1373bhxaf0pagd8zkyd03kkx6bchzf6g0dkwrwzsnal9z47lj9fs"))

(define rust-proc-macro-error-attr-1.0.4
  (crate-source "proc-macro-error-attr" "1.0.4"
                "0sgq6m5jfmasmwwy8x4mjygx5l7kp8s4j60bv25ckv2j1qc41gm1"))

(define rust-proc-macro2-1.0.101
  (crate-source "proc-macro2" "1.0.101"
                "1pijhychkpl7rcyf1h7mfk6gjfii1ywf5n0snmnqs5g4hvyl7bl9"))

(define rust-proc-macro2-1.0.103
  (crate-source "proc-macro2" "1.0.103"
                "1s29bz20xl2qk5ffs2mbdqknaj43ri673dz86axdbf47xz25psay"))

(define rust-proc-macro2-1.0.105
  (crate-source "proc-macro2" "1.0.105"
                "1rvgs5qdznlrqrgicmv24nybnrnv8kyvk2vi7s52ddna1q71hpak"))

(define rust-proc-macro2-1.0.76
  (crate-source "proc-macro2" "1.0.76"
                "136cp0fgl6rg5ljm3b1xpc0bn0lyvagzzmxvbxgk5hxml36mdz4m"))

(define rust-proc-macro2-1.0.78
  (crate-source "proc-macro2" "1.0.78"
                "1bjak27pqdn4f4ih1c9nr3manzyavsgqmf76ygw9k76q8pb2lhp2"))

(define rust-proc-macro2-1.0.86
  (crate-source "proc-macro2" "1.0.86"
                "0xrv22p8lqlfdf1w0pj4si8n2ws4aw0kilmziwf0vpv5ys6rwway"))

(define rust-proc-macro2-1.0.92
  (crate-source "proc-macro2" "1.0.92"
                "1c1vjy5wg8iy7kxsxda564qf4ljp0asysmbn2i7caj177x5m9lrp"))

(define rust-proc-macro2-1.0.93
  (crate-source "proc-macro2" "1.0.93"
                "169dw9wch753if1mgyi2nfl1il77gslvh6y2q46qplprwml6m530"))

(define rust-proc-macro2-1.0.94
  (crate-source "proc-macro2" "1.0.94"
                "114wxb56gdj9vy44q0ll3l2x9niqzcbyqikydmlb5f3h5rsp26d3"))

(define rust-proc-macro2-diagnostics-0.10.1
  (crate-source "proc-macro2-diagnostics" "0.10.1"
                "1j48ipc80pykvhx6yhndfa774s58ax1h6sm6mlhf09ls76f6l1mg"))

(define rust-proc-status-0.1.1
  (crate-source "proc-status" "0.1.1"
                "04lp8kdj75m8s1hwxslyzz3fdgbs6zy4zfjhg2s7cysyj6nc1q7h"))

(define rust-process-lib-1.0.0
  (crate-source "process-lib" "1.0.0"
                "0ilcj6bzg0ncg5hi8jmc5l7p9j76vmb6zpmvxhnb7xpa7b9pxd1y"))

(define rust-prodash-29.0.1
  (crate-source "prodash" "29.0.1"
                "12xm50jzkqzdqdcidmzy4d6rj9r8x6mf8sidgrh7dfc0r4jcxrwy"))

(define rust-prodash-30.0.1
  (crate-source "prodash" "30.0.1"
                "0fdi0wxgy3s9643dgyfkwgmm12g4a360djy56zbxkls9d1bgqvjs"))

(define rust-profiling-1.0.16
  (crate-source "profiling" "1.0.16"
                "0kcz2xzg4qx01r5az8cf9ffjasi2srj56sna32igddh0vi7cggdg"))

(define rust-profiling-1.0.17
  (crate-source "profiling" "1.0.17"
                "0wqp6i1bl7azy9270dp92srbbr55mgdh9qnk5b1y44lyarmlif1y"))

(define rust-profiling-procmacros-1.0.17
  (crate-source "profiling-procmacros" "1.0.17"
                "0nrxdh5r723raxbs136jmjx46p0c5qgai8jwz4j555mn0ad7ywaj"))

(define rust-proptest-1.9.0
  (crate-source "proptest" "1.9.0"
                "0h4cprpwir4dkxfamj1iz6qx3cp38gp552rln1ma3l1b7928krmy"))

(define rust-proptest-state-machine-0.6.0
  (crate-source "proptest-state-machine" "0.6.0"
                "1pwkhb6ib585czm4q4pwvq1hwqamqjk3algz9kc51a8g0pxrpfky"))

(define rust-prost-0.12.6
  (crate-source "prost" "0.12.6"
                "0a8z87ir8yqjgl1kxbdj30a7pzsjs9ka85szll6i6xlb31f47cfy"))

(define rust-prost-0.14.1
  (crate-source "prost" "0.14.1"
                "0gazm7m6yqvksw0jilhrdd4rzbf0br5wgfmdb1mwhcrx7ndvscbj"))

(define rust-prost-derive-0.12.6
  (crate-source "prost-derive" "0.12.6"
                "1waaq9d2f114bvvpw957s7vsx268licnfawr20b51ydb43dxrgc1"))

(define rust-prost-derive-0.14.1
  (crate-source "prost-derive" "0.14.1"
                "0994czxnv69jnchcrr25rk4vp77cs0kzagc0ldxsd2f3mw7nj84i"))

(define rust-ptr-meta-0.1.4
  (crate-source "ptr_meta" "0.1.4"
                "1wd4wy0wxrcays4f1gy8gwcmxg7mskmivcv40p0hidh6xbvwqf07"))

(define rust-ptr-meta-derive-0.1.4
  (crate-source "ptr_meta_derive" "0.1.4"
                "1b69cav9wn67cixshizii0q5mlbl0lihx706vcrzm259zkdlbf0n"))

(define rust-pure-rust-locales-0.8.1
  (crate-source "pure-rust-locales" "0.8.1"
                "0fkkwggiq2053rmiah2h06dz6w3yhy9pa82g30vy3sbcmqcgv40i"))

(define rust-pxfm-0.1.24
  (crate-source "pxfm" "0.1.24"
                "1phz82xji3yr5bypdqbwl4x96bs7zfvqk8zlq2jssn92n0wv7yc3"))

(define rust-pxfm-0.1.25
  (crate-source "pxfm" "0.1.25"
                "114x01bxzdlwbzjr2n7vs73y4f1r003qsl9v9pgpigvj74vxzjx3"))

(define rust-pxfm-0.1.27
  (crate-source "pxfm" "0.1.27"
                "1a76ydn3wpl2dvyzplv3c6fkx4mkjc9ns60xas9l7alk4n1d71ki"))

(define rust-qoi-0.4.1
  (crate-source "qoi" "0.4.1"
                "00c0wkb112annn2wl72ixyd78mf56p4lxkhlmsggx65l3v3n8vbz"))

(define rust-quanta-0.12.3
  (crate-source "quanta" "0.12.3"
                "19cds3yg3ri0wrypn7b3j2x8qf1w9rkw5yl4nah2i4k1fyj6flcf"))

(define rust-quick-error-1.2.3
  (crate-source "quick-error" "1.2.3"
                "1q6za3v78hsspisc197bg3g7rpc989qycy8ypr8ap8igv10ikl51"))

(define rust-quick-error-2.0.1
  (crate-source "quick-error" "2.0.1"
                "18z6r2rcjvvf8cn92xjhm2qc3jpd1ljvcbf12zv0k9p565gmb4x9"))

(define rust-quick-xml-0.30.0
  (crate-source "quick-xml" "0.30.0"
                "0mp9cqy06blsaka3r1n2p40ddmzhsf7bx37x22r5faw6hq753xpg"))

(define rust-quick-xml-0.36.2
  (crate-source "quick-xml" "0.36.2"
                "1zj3sjcjk6sn544wb2wvhr1km5f9cy664vzclygfsnph9mxrlr7p"))

(define rust-quick-xml-0.37.2
  (crate-source "quick-xml" "0.37.2"
                "00y0qagwbxd3lqarr13j35d6kwmni176znf5jrxxcyazwplmjn0n"))

(define rust-quick-xml-0.37.5
  (crate-source "quick-xml" "0.37.5"
                "1yxpd7rc2qn6f4agfj47ps2z89vv7lvzxpzawqirix8bmyhrf7ik"))

(define rust-quick-xml-0.38.3
  (crate-source "quick-xml" "0.38.3"
                "12bvsbnnmlnq9xg9in3h3080ag3sisafgpcn7lqyzhkz93kk58j2"))

(define rust-quinn-0.11.6
  (crate-source "quinn" "0.11.6"
                "1vq55p4kfc4zjxj58xrpf3kcjjqi4mn0wf52a5rzkiky4w46isb2"))

(define rust-quinn-0.11.9
  (crate-source "quinn" "0.11.9"
                "086gzj666dr3slmlynkvxlndy28hahgl361d6bf93hk3i6ahmqmr"))

(define rust-quinn-proto-0.11.13
  (crate-source "quinn-proto" "0.11.13"
                "0cca3mgja9p4w66f6sl1kfhj8rdf4mwsg1jxzssh9g63n14np47i"))

(define rust-quinn-proto-0.11.9
  (crate-source "quinn-proto" "0.11.9"
                "0p8k3iqd0rcxc7b6m2yyijhw4bpfwa61lyzigwvjwzax97rmxzm2"))

(define rust-quinn-udp-0.5.14
  (crate-source "quinn-udp" "0.5.14"
                "1gacawr17a2zkyri0r3m0lc9spzmxbq1by3ilyb8v2mdvjhcdpmd"))

(define rust-quinn-udp-0.5.9
  (crate-source "quinn-udp" "0.5.9"
                "016rir0w5q6brwd33id2nvqi67xpl3k55ms4srqkmfml2xi2hh0w"))

(define rust-quote-0.3.15
  (crate-source "quote" "0.3.15"
                "0yhnnix4dzsv8y4wwz4csbnqjfh73al33j35msr10py6cl5r4vks"))

(define rust-quote-1.0.35
  (crate-source "quote" "1.0.35"
                "1vv8r2ncaz4pqdr78x7f138ka595sp2ncr1sa2plm4zxbsmwj7i9"))

(define rust-quote-1.0.36
  (crate-source "quote" "1.0.36"
                "19xcmh445bg6simirnnd4fvkmp6v2qiwxh5f6rw4a70h76pnm9qg"))

(define rust-quote-1.0.38
  (crate-source "quote" "1.0.38"
                "1k0s75w61k6ch0rs263r4j69b7vj1wadqgb9dia4ylc9mymcqk8f"))

(define rust-quote-1.0.39
  (crate-source "quote" "1.0.39"
                "00a8q2w3aacil4aqnndyv73k0x4lj55kp487k66nbq89x5693wf1"))

(define rust-quote-1.0.40
  (crate-source "quote" "1.0.40"
                "1394cxjg6nwld82pzp2d4fp6pmaz32gai1zh9z5hvh0dawww118q"))

(define rust-quote-1.0.41
  (crate-source "quote" "1.0.41"
                "1lg108nb57lwbqlnpsii89cchk6i8pkcvrv88xh1p7a9gdz7c9ff"))

(define rust-quote-1.0.42
  (crate-source "quote" "1.0.42"
                "0zq6yc7dhpap669m27rb4qfbiywxfah17z6fwvfccv3ys90wqf53"))

(define rust-quote-1.0.43
  (crate-source "quote" "1.0.43"
                "02n41mlr81qmczac7m5kjy51y8b7yrb8ym4ncmjycampjjjxjx6w"))

(define rust-r-efi-5.2.0
  (crate-source "r-efi" "5.2.0"
                "1ig93jvpqyi87nc5kb6dri49p56q7r7qxrn8kfizmqkfj5nmyxkl"))

(define rust-r-efi-5.3.0
  (crate-source "r-efi" "5.3.0"
                "03sbfm3g7myvzyylff6qaxk4z6fy76yv860yy66jiswc2m6b7kb9"))

(define rust-radium-0.7.0
  (crate-source "radium" "0.7.0"
                "02cxfi3ky3c4yhyqx9axqwhyaca804ws46nn4gc1imbk94nzycyw"))

(define rust-rand-0.4.6
  (crate-source "rand" "0.4.6"
                "14qjfv3gggzhnma20k0sc1jf8y6pplsaq7n1j9ls5c8kf2wl0a2m"))

(define rust-rand-0.8.5
  (crate-source "rand" "0.8.5"
                "013l6931nn7gkc23jz5mm3qdhf93jjf0fg64nz2lp4i51qd8vbrl"))

(define rust-rand-0.9.2
  (crate-source "rand" "0.9.2"
                "1lah73ainvrgl7brcxx0pwhpnqa3sm3qaj672034jz8i0q7pgckd"))

(define rust-rand-chacha-0.3.1
  (crate-source "rand_chacha" "0.3.1"
                "123x2adin558xbhvqb8w4f6syjsdkmqff8cxwhmjacpsl1ihmhg6"))

(define rust-rand-chacha-0.9.0
  (crate-source "rand_chacha" "0.9.0"
                "1jr5ygix7r60pz0s1cv3ms1f6pd1i9pcdmnxzzhjc3zn3mgjn0nk"))

(define rust-rand-core-0.3.1
  (crate-source "rand_core" "0.3.1"
                "0jzdgszfa4bliigiy4hi66k7fs3gfwi2qxn8vik84ph77fwdwvvs"))

(define rust-rand-core-0.4.2
  (crate-source "rand_core" "0.4.2"
                "1p09ynysrq1vcdlmcqnapq4qakl2yd1ng3kxh3qscpx09k2a6cww"))

(define rust-rand-core-0.6.4
  (crate-source "rand_core" "0.6.4"
                "0b4j2v4cb5krak1pv6kakv4sz6xcwbrmy2zckc32hsigbrwy82zc"))

(define rust-rand-core-0.9.3
  (crate-source "rand_core" "0.9.3"
                "0f3xhf16yks5ic6kmgxcpv1ngdhp48mmfy4ag82i1wnwh8ws3ncr"))

(define rust-rand-core-0.9.5
  (crate-source "rand_core" "0.9.5"
                "0g6qc5r3f0hdmz9b11nripyp9qqrzb0xqk9piip8w8qlvqkcibvn"))

(define rust-rand-regex-0.18.1
  (crate-source "rand_regex" "0.18.1"
                "0dyzikkp5bjfp8sv06ylbkf7kfyd0vir03j088g9lrqk5lw2xnq4"))

(define rust-rand-xorshift-0.4.0
  (crate-source "rand_xorshift" "0.4.0"
                "0njsn25pis742gb6b89cpq7jp48v9n23a9fvks10yczwks8n4fai"))

(define rust-range-alloc-0.1.3
  (crate-source "range-alloc" "0.1.3"
                "1azfwh89nd4idj0s272qgmw3x1cj6m7d3f44b2la02wzvkyrk2lw"))

(define rust-range-alloc-0.1.4
  (crate-source "range-alloc" "0.1.4"
                "1plvrb6gaaa5in2fjv67wgs9aki8qrczz77qcjhqw2d5ccb87mn3"))

(define rust-rangemap-1.5.1
  (crate-source "rangemap" "1.5.1"
                "116nhgzbjil97qjq3d64mg7fi7lqka5hqdf44bzjwd29d1ywq3zn"))

(define rust-rangemap-1.6.0
  (crate-source "rangemap" "1.6.0"
                "08r2qagsc9k02qw30zpc91ghqsrxid2p9mkvgxqngy8bpd4pwgpr"))

(define rust-ratatui-0.29.0
  (crate-source "ratatui" "0.29.0"
                "0yqiccg1wmqqxpb2sz3q2v3nifmhsrfdsjgwhc2w40bqyg199gga"))

(define rust-ratatui-0.30.0
  (crate-source "ratatui" "0.30.0"
                "1g36h96fnr8ay7bmwplxsfa5xzsp0pdaxny8s5a68i54igxngkni"))

(define rust-ratatui-core-0.1.0
  (crate-source "ratatui-core" "0.1.0"
                "14y2pv5njy7kpzjsfn20a8vmjbhnfq5vgbgppxrszjljkahdxy2y"))

(define rust-ratatui-crossterm-0.1.0
  (crate-source "ratatui-crossterm" "0.1.0"
                "1cslvh75a29gdmz84s5sjaqd61k4s0fkjsjwn8gi4k1bcngrnz2p"))

(define rust-ratatui-macros-0.7.0
  (crate-source "ratatui-macros" "0.7.0"
                "1x1nr4wyyhchms9chj10b3wk7ribfvmd14xps2wlngp82cm39wd7"))

(define rust-ratatui-termwiz-0.1.0
  (crate-source "ratatui-termwiz" "0.1.0"
                "0p2l7pymlcfv9n802pd32m604m145rrpc5hv6bq9ahpds05zwxhg"))

(define rust-ratatui-widgets-0.3.0
  (crate-source "ratatui-widgets" "0.3.0"
                "1nqjcrskazvfgjkmmsifliqrvap8bw6850rlap109rnl7h1gmnyp"))

(define rust-rav1e-0.7.1
  (crate-source "rav1e" "0.7.1"
                "1sawva6nmj2fvynydbcirr3nb7wjyg0id2hz2771qnv6ly0cx1yd"))

(define rust-ravif-0.11.20
  (crate-source "ravif" "0.11.20"
                "0sryi4qzwv3rjhy4h0z9fb0zw3k3i814j3ck2psvjsnivmpw49aq"))

(define rust-raw-cpuid-11.2.0
  (crate-source "raw-cpuid" "11.2.0"
                "1c77cmsn7rj6knwwrg2y9nl46wss5p9jq3wzxvr1a5k6bhql1chs"))

(define rust-raw-window-handle-0.6.2
  (crate-source "raw-window-handle" "0.6.2"
                "0ff5c648hncwx7hm2a8fqgqlbvbl4xawb6v3xxv9wkpjyrr5arr0"))

(define rust-rayon-1.10.0
  (crate-source "rayon" "1.10.0"
                "1ylgnzwgllajalr4v00y4kj22klq2jbwllm70aha232iah0sc65l"))

(define rust-rayon-1.11.0
  (crate-source "rayon" "1.11.0"
                "13x5fxb7rn4j2yw0cr26n7782jkc7rjzmdkg42qxk3xz0p8033rn"))

(define rust-rayon-1.7.0
  (crate-source "rayon" "1.7.0"
                "0fzh8w5ds1qjhilll4rkpd3kimw70zi5605wprxcig1pdqczab8x"))

(define rust-rayon-core-1.11.0
  (crate-source "rayon-core" "1.11.0"
                "13dymrhhdilzpbfh3aylv6ariayqdfk614b3frvwixb6d6yrb3sb"))

(define rust-rayon-core-1.12.1
  (crate-source "rayon-core" "1.12.1"
                "1qpwim68ai5h0j7axa8ai8z0payaawv3id0lrgkqmapx7lx8fr8l"))

(define rust-rayon-core-1.13.0
  (crate-source "rayon-core" "1.13.0"
                "14dbr0sq83a6lf1rfjq5xdpk5r6zgzvmzs5j6110vlv2007qpq92"))

(define rust-rcgen-0.14.6
  (crate-source "rcgen" "0.14.6"
                "0zwhal2gqdmjgrj7yq057rs6png4h3dyfdsbr3fkq7795ngskh1y"))

(define rust-rdrand-0.4.0
  (crate-source "rdrand" "0.4.0"
                "1cjq0kwx1bk7jx3kzyciiish5gqsj7620dm43dc52sr8fzmm9037"))

(define rust-read-fonts-0.22.7
  (crate-source "read-fonts" "0.22.7"
                "0bzc5qyp1a98x4q5hfa9pfp9d552b6bx6m81z6vwz6n2nmvcpak9"))

(define rust-read-fonts-0.31.3
  (crate-source "read-fonts" "0.31.3"
                "0152xwjwn7lri8vf469nis7kxw5iww86zq3ijaxbkm4yy2w510jv"))

(define rust-read-fonts-0.35.0
  (crate-source "read-fonts" "0.35.0"
                "0n0kr71apps034i708r6vx3y2kd35da2j4vikm5zdbc8nhiwy5v7"))

(define rust-redox-syscall-0.2.16
  (crate-source "redox_syscall" "0.2.16"
                "16jicm96kjyzm802cxdd1k9jmcph0db1a4lhslcnhjsvhp0mhnpv"))

(define rust-redox-syscall-0.3.5
  (crate-source "redox_syscall" "0.3.5"
                "0acgiy2lc1m2vr8cr33l5s7k9wzby8dybyab1a9p753hcbr68xjn"))

(define rust-redox-syscall-0.4.1
  (crate-source "redox_syscall" "0.4.1"
                "1aiifyz5dnybfvkk4cdab9p2kmphag1yad6iknc7aszlxxldf8j7"))

(define rust-redox-syscall-0.5.10
  (crate-source "redox_syscall" "0.5.10"
                "1l9b638qx72312yzh8ykvda9b3lqd9gf6yqn66b23a331ck0r30b"))

(define rust-redox-syscall-0.5.11
  (crate-source "redox_syscall" "0.5.11"
                "18qijn18r10haiglv4261wb0yh1agqqlvs0nxfy8yjbpsb307wfj"))

(define rust-redox-syscall-0.5.17
  (crate-source "redox_syscall" "0.5.17"
                "0xrvpchkaxph3r5ww2i04v9nwg3843fp3prf8kqlh1gv01b4c1sl"))

(define rust-redox-syscall-0.5.18
  (crate-source "redox_syscall" "0.5.18"
                "0b9n38zsxylql36vybw18if68yc9jczxmbyzdwyhb9sifmag4azd"))

(define rust-redox-syscall-0.5.2
  (crate-source "redox_syscall" "0.5.2"
                "1zg03n8rjbqnnbzp5nvai2pjgzxf7cw42hpcamgh8mj4y77zhb68"))

(define rust-redox-syscall-0.5.3
  (crate-source "redox_syscall" "0.5.3"
                "1916m7abg9649gkif055pn5nsvqjhp70isy0v7gx1zgi01p8m41a"))

(define rust-redox-syscall-0.5.8
  (crate-source "redox_syscall" "0.5.8"
                "0d48ylyd6gsamynyp257p6n2zl4dw2fhnn5z9y3nhgpri6rn5a03"))

(define rust-redox-users-0.4.3
  (crate-source "redox_users" "0.4.3"
                "0asw3s4iy69knafkhvlbchy230qawc297vddjdwjs5nglwvxhcxh"))

(define rust-redox-users-0.4.6
  (crate-source "redox_users" "0.4.6"
                "0hya2cxx6hxmjfxzv9n8rjl5igpychav7zfi1f81pz6i4krry05s"))

(define rust-redox-users-0.5.0
  (crate-source "redox_users" "0.5.0"
                "0awxx66izdw6kz97r3zxrl5ms5f6dqi5l0f58mlsvlmx8wyrsvyx"))

(define rust-redox-users-0.5.2
  (crate-source "redox_users" "0.5.2"
                "1b17q7gf7w8b1vvl53bxna24xl983yn7bd00gfbii74bcg30irm4"))

(define rust-ref-cast-1.0.24
  (crate-source "ref-cast" "1.0.24"
                "1kx57g118vs9sqi6d2dcxy6vp8jbx8n5hilmv1sacip9vc8y82ja"))

(define rust-ref-cast-1.0.25
  (crate-source "ref-cast" "1.0.25"
                "0zdzc34qjva9xxgs889z5iz787g81hznk12zbk4g2xkgwq530m7k"))

(define rust-ref-cast-impl-1.0.24
  (crate-source "ref-cast-impl" "1.0.24"
                "1ir7dm7hpqqdgg60hlspsc1ck6wli7wa3xcqrsxz7wdz45f24r8i"))

(define rust-ref-cast-impl-1.0.25
  (crate-source "ref-cast-impl" "1.0.25"
                "1nkhn1fklmn342z5c4mzfzlxddv3x8yhxwwk02cj06djvh36065p"))

(define rust-referencing-0.33.0
  (crate-source "referencing" "0.33.0"
                "0zb9k0q7ijx2zx2vmpj0jvmyi7r9n473zyxdpsrihm77ghgjd74y"))

(define rust-referencing-0.37.4
  (crate-source "referencing" "0.37.4"
                "036ps93bgz07czj4djkb9kd29j996779r7qwwgfcn3bga251d0s2"))

(define rust-regex-1.10.3
  (crate-source "regex" "1.10.3"
                "05cvihqy0wgnh9i8a9y2n803n5azg2h0b7nlqy6rsvxhy00vwbdn"))

(define rust-regex-1.10.5
  (crate-source "regex" "1.10.5"
                "0zsiqk2sxc1kd46qw0yp87s2a14ialwyxinpl0k266ddkm1i64mr"))

(define rust-regex-1.10.6
  (crate-source "regex" "1.10.6"
                "06cnlxwzyqfbw1za1i7ks89ns4i2kr0lpg5ykx56b8v7dd6df6a2"))

(define rust-regex-1.11.1
  (crate-source "regex" "1.11.1"
                "148i41mzbx8bmq32hsj1q4karkzzx5m60qza6gdw4pdc9qdyyi5m"))

(define rust-regex-1.11.3
  (crate-source "regex" "1.11.3"
                "0b58ya98c4i5cjjiwhpcnjr61cv9g143qhdwhsryggj09098hllb"))

(define rust-regex-1.12.2
  (crate-source "regex" "1.12.2"
                "1m14zkg6xmkb0q5ah3y39cmggclsjdr1wpxfa4kf5wvm3wcw0fw4"))

(define rust-regex-automata-0.1.10
  (crate-source "regex-automata" "0.1.10"
                "0ci1hvbzhrfby5fdpf4ganhf7kla58acad9i1ff1p34dzdrhs8vc"))

(define rust-regex-automata-0.3.9
  (crate-source "regex-automata" "0.3.9"
                "1agg6ymbgjydj3q31ay6dbzgp3i5cnrnygpylczqj623xs93xcjr"))

(define rust-regex-automata-0.4.11
  (crate-source "regex-automata" "0.4.11"
                "1bawj908pxixpggcnma3xazw53mwyz68lv9hn4yg63nlhv7bjgl3"))

(define rust-regex-automata-0.4.13
  (crate-source "regex-automata" "0.4.13"
                "070z0j23pjfidqz0z89id1fca4p572wxpcr20a0qsv68bbrclxjj"))

(define rust-regex-automata-0.4.4
  (crate-source "regex-automata" "0.4.4"
                "0akrxca6lw02y8fgvlypfg24dkfsz0bnnhgxaf9yrqh58h9s2zrv"))

(define rust-regex-automata-0.4.5
  (crate-source "regex-automata" "0.4.5"
                "1karc80mx15z435rm1jg3sqylnc58nxi15gqypcd1inkzzpqgfav"))

(define rust-regex-automata-0.4.7
  (crate-source "regex-automata" "0.4.7"
                "1pwjdi4jckpbaivpl6x4v5g4crb37zr2wac93wlfsbzgqn6gbjiq"))

(define rust-regex-automata-0.4.9
  (crate-source "regex-automata" "0.4.9"
                "02092l8zfh3vkmk47yjc8d631zhhcd49ck2zr133prvd3z38v7l0"))

(define rust-regex-lite-0.1.8
  (crate-source "regex-lite" "0.1.8"
                "1njm055j5kfq0cqc6ray24wgwcw8hrzjqn8dy9b8yrayvyc2p54d"))

(define rust-regex-syntax-0.6.29
  (crate-source "regex-syntax" "0.6.29"
                "1qgj49vm6y3zn1hi09x91jvgkl2b1fiaq402skj83280ggfwcqpi"))

(define rust-regex-syntax-0.7.5
  (crate-source "regex-syntax" "0.7.5"
                "1nhjmqdlakfi4yb8lh7vbbh71dsy90jjvrjvvnrih6larldgpdfv"))

(define rust-regex-syntax-0.8.2
  (crate-source "regex-syntax" "0.8.2"
                "17rd2s8xbiyf6lb4aj2nfi44zqlj98g2ays8zzj2vfs743k79360"))

(define rust-regex-syntax-0.8.4
  (crate-source "regex-syntax" "0.8.4"
                "16r0kjy20vx33dr4mhasj5l1f87czas714x2fz6zl0f8wwxa0rks"))

(define rust-regex-syntax-0.8.5
  (crate-source "regex-syntax" "0.8.5"
                "0p41p3hj9ww7blnbwbj9h7rwxzxg0c1hvrdycgys8rxyhqqw859b"))

(define rust-regex-syntax-0.8.6
  (crate-source "regex-syntax" "0.8.6"
                "00chjpglclfskmc919fj5aq308ffbrmcn7kzbkz92k231xdsmx6a"))

(define rust-regex-syntax-0.8.8
  (crate-source "regex-syntax" "0.8.8"
                "0n7ggnpk0r32rzgnycy5xrc1yp2kq19m6pz98ch3c6dkaxw9hbbs"))

(define rust-relative-path-1.9.3
  (crate-source "relative-path" "1.9.3"
                "1limlh8fzwi21g0473fqzd6fln9iqkwvzp3816bxi31pkilz6fds"))

(define rust-remove-dir-all-0.5.3
  (crate-source "remove_dir_all" "0.5.3"
                "1rzqbsgkmr053bxxl04vmvsd1njyz0nxvly97aip6aa2cmb15k9s"))

(define rust-rend-0.4.2
  (crate-source "rend" "0.4.2"
                "0z4rrkycva0lcw0hxq479h4amxj9syn5vq4vb2qid5v2ylj3izki"))

(define rust-renderdoc-sys-1.1.0
  ;; TODO: Check bundled sources.
  (crate-source "renderdoc-sys" "1.1.0"
                "0cj8zjs7k0gvchcx3jhpg8r9bbqy8b1hsgbz0flcq2ydn12hmcqr"))

(define rust-reqwest-0.11.27
  (crate-source "reqwest" "0.11.27"
                "0qjary4hpplpgdi62d2m0xvbn6lnzckwffm0rgkm2x51023m6ryx"))

(define rust-reqwest-0.12.12
  (crate-source "reqwest" "0.12.12"
                "1nnigi6jcrqdd5k5myc53qdkdnrx8zjgan029q1w5hspf5039rs3"))

(define rust-reqwest-0.12.24
  (crate-source "reqwest" "0.12.24"
                "0vx3f2n6hfnv81y66v5wayrqh6jlzz4gakky88m0hywz1d0lc2cx"))

(define rust-reqwest-0.12.25
  (crate-source "reqwest" "0.12.25"
                "06khlxs7xw9pkvyawk9a0097795nkvbl47cipm1is4s0ilrgkvxn"))

(define rust-reqwest-0.12.28
  (crate-source "reqwest" "0.12.28"
                "0iqidijghgqbzl3bjg5hb4zmigwa4r612bgi0yiq0c90b6jkrpgd"))

(define rust-reqwest-0.12.5
  (crate-source "reqwest" "0.12.5"
                "0dva5mx1cli19adc9igc81ix4si7yiri24ayxdw8652pgnid5mn7"))

(define rust-resize-slice-0.1.3
  (crate-source "resize-slice" "0.1.3"
                "0jlc614745fvf7ykxfz379n4cnx2d79crq5rb1lyg4cq9bvv4g4a"))

(define rust-resolv-conf-0.7.0
  (crate-source "resolv-conf" "0.7.0"
                "005sk8r1php2g41yn7fdf1sn8cafyaqm6jxia42h2v88saa47r2j"))

(define rust-resolv-conf-0.7.6
  (crate-source "resolv-conf" "0.7.6"
                "1ivhckfhn7zksvid506w8dhrs001cix0mbp45l23i3fb90dis1hy"))

(define rust-resvg-0.42.0
  (crate-source "resvg" "0.42.0"
                "0lchbn00633ir84732583yqa0nz94190bb7afzxchshm2ll0akcl"))

(define rust-resvg-0.45.1
  (crate-source "resvg" "0.45.1"
                "0hyz89wyasn0wdnpw7qy5iyl4xv3yx36hk3crb4h6pm5q2c8g4m8"))

(define rust-rfc6979-0.4.0
  (crate-source "rfc6979" "0.4.0"
                "1chw95jgcfrysyzsq6a10b1j5qb7bagkx8h0wda4lv25in02mpgq"))

(define rust-rfd-0.15.1
  (crate-source "rfd" "0.15.1"
                "1s01pnp9w60f96sd5vpja2bzq3yk4ncwlws607clf9l8kc5gixj6"))

(define rust-rfd-0.15.4
  (crate-source "rfd" "0.15.4"
                "1vgbfi32843dysqdq3fv604qzqll8al82z9dqwsldyngwrhywazg"))

(define rust-rgb-0.8.50
  (crate-source "rgb" "0.8.50"
                "02ii3nsciska0sj23ggxaz8gj64ksw8nbpfjcwxlh037chb7sfap"))

(define rust-rgb-0.8.52
  (crate-source "rgb" "0.8.52"
                "1km115a9lblf9pldvx51dmmg30y8ms4ka67hvas2ndcq556qhshc"))

(define rust-ring-0.17.14
  (crate-source "ring" "0.17.14"
                "1dw32gv19ccq4hsx3ribhpdzri1vnrlcfqb2vj41xn4l49n9ws54"))

(define rust-ring-0.17.3
  (crate-source "ring" "0.17.3"
                "0zhx35zyc07pipg7f2z6ga74j27yx0msscmajijwzghnbh6yiawv"))

(define rust-ring-0.17.8
  (crate-source "ring" "0.17.8"
                "03fwlb1ssrmfxdckvqv033pfmk01rhx9ynwi7r186dcfcp5s8zy1"))

(define rust-rip-starttls-0.1.0
  (crate-source "rip-starttls" "0.1.0"
                "04m15c2067byynyzkpg2fvav86si1vi6671203g7n4v5g4ngdwgd"))

(define rust-ripemd-0.1.3
  (crate-source "ripemd" "0.1.3"
                "17xh5yl9wjjj2v18rh3m8ajlmdjg1yj13l6r9rj3mnbss4i444mx"))

(define rust-rkyv-0.7.46
  (crate-source "rkyv" "0.7.46"
                "18fngrp1kzsmkkl7asl25661cm7hi05kf8cmpjbdrw53h6fbz5r2"))

(define rust-rkyv-derive-0.7.46
  (crate-source "rkyv_derive" "0.7.46"
                "1x9q626kkppbnbrbbw09nyz2r56b3frhxny87a6h81ld9cnv9mw4"))

(define rust-rlimit-0.10.2
  (crate-source "rlimit" "0.10.2"
                "0fkr1jyhg0ch50kryy1jxd83nfh2skk80ss7irial6nds0xvchvh"))

(define rust-rodio-0.20.1
  (crate-source "rodio" "0.20.1"
                "1c8g2h6lih880rnbk7cbr275rkdp97rfya6br2dwjf6pgmhbdkp7"))

(define rust-rodio-0.21.1
  (crate-source "rodio" "0.21.1"
                "10z134ip496n1vx2v8lz0mdd17vqbrsm6gbapqv37q22wxcwy3p4"))

(define rust-roff-0.2.2
  (crate-source "roff" "0.2.2"
                "1wyqz6m0pm4p6wzhwhahvcidfm7nwb38zl4q7ha940pn3w66dy48"))

(define rust-ron-0.8.1
  (crate-source "ron" "0.8.1"
                "154w53s895yxdfg7rn87c6f6x4yncc535x1x31zpcj7p0pzpw7xr"))

(define rust-roxmltree-0.20.0
  (crate-source "roxmltree" "0.20.0"
                "15vw91ps91wkmmgy62khf9zb63bdinvm80957dascbsw7dwvc83c"))

(define rust-roxmltree-0.21.0
  (crate-source "roxmltree" "0.21.0"
                "0gzmajlaxffndf7d39aq6znlqb8a22pkr2kvlpq7bscm0xx3wgln"))

(define rust-rpassword-7.3.1
  (crate-source "rpassword" "7.3.1"
                "0gvy3lcpph9vv1rl0cjfn72ylvmgbw2vklmj6w0iv4cpr3ijniw0"))

(define rust-rpassword-7.4.0
  (crate-source "rpassword" "7.4.0"
                "0ffzfff51pl95a7px9gwlz243mn3vxyw7klcxhhng7049yvcim36"))

(define rust-rsa-0.9.7
  (crate-source "rsa" "0.9.7"
                "06amqm85raq26v6zg00fbf93lbj3kx559n2lpxc3wrvbbiy5vis7"))

(define rust-rspirv-0.12.0+sdk-1.3.268.0
  (crate-source "rspirv" "0.12.0+sdk-1.3.268.0"
                "13gcmxz0yazh2bqcy34mn5qn6nbm606xyy3jad35jvkbhn9kmkv9"))

(define rust-rstest-0.24.0
  (crate-source "rstest" "0.24.0"
                "12fd0cnd49n6dnlmygca97lkakvc9czs1hqy7khr7aq5d0lhbs83"))

(define rust-rstest-0.26.1
  (crate-source "rstest" "0.26.1"
                "0jcxhg9mxlr2p9an14algbcq6ax7r0sk1w1kbals5aiv0qy1k8zm"))

(define rust-rstest-macros-0.24.0
  (crate-source "rstest_macros" "0.24.0"
                "16zgrnnwgm6qss4f1vzig6w5fymy1ydlkk2bxqmhc2ffzyxm607g"))

(define rust-rstest-macros-0.26.1
  (crate-source "rstest_macros" "0.26.1"
                "185v185wn2x3llp3nn1i7h44vi5ffnnsj8b1a32m2ygzy08m714w"))

(define rust-rstest-reuse-0.7.0
  (crate-source "rstest_reuse" "0.7.0"
                "057y4v1rh9br58n2m3xqvm8xyx8k96jpgibgls3sah78f93gpa5k"))

(define rust-rtoolbox-0.0.2
  (crate-source "rtoolbox" "0.0.2"
                "03n9z8x353kylxhr9im8zawcisnmid3jiqrs8rbdn313cd7d4iy2"))

(define rust-rtoolbox-0.0.3
  (crate-source "rtoolbox" "0.0.3"
                "0vvz9p8wdzspwd7hk0cxyjr0i49cfqks5q02drym5glz4h5rgk57"))

(define rust-rusqlite-0.32.1
  (crate-source "rusqlite" "0.32.1"
                "0vlx040bppl414pbjgbp7qr4jdxwszi9krx0m63zzf2f2whvflvp"))

(define rust-rusqlite-0.37.0
  (crate-source "rusqlite" "0.37.0"
                "0gqzwykyfaaddq5rg1jk0940wby6ifarnwp3fcakbq90ggjscp0n"))

(define rust-rust-decimal-1.40.0
  (crate-source "rust_decimal" "1.40.0"
                "1c1yb8lms5aqzlaarwa0d7mn30s2h7x46djipiyginsjk38h7xv1"))

(define rust-rust-fuzzy-search-0.1.1
  (crate-source "rust-fuzzy-search" "0.1.1"
                "1chvl47hq42r219yxs6r1dp4l19acy5ay145hpc5drgzaiq6amx1"))

(define rust-rust-ini-0.18.0
  (crate-source "rust-ini" "0.18.0"
                "1px22l3m84v7f46pa3p4bsjykivw8ryq6af8kpkzdd16c11z5mgn"))

(define rust-rust-strings-0.6.0
  (crate-source "rust-strings" "0.6.0"
                "0xhlvbghbvvyxzmpipyw02lwwxrix9rj5y42jklsgc14xam5jxfi"))

(define rust-rustc-demangle-0.1.22
  (crate-source "rustc-demangle" "0.1.22"
                "0nrfs6g3hb7xk13mp904qqdqknb4jr7hkpivqmvrlgw7s516r8yl"))

(define rust-rustc-demangle-0.1.23
  (crate-source "rustc-demangle" "0.1.23"
                "0xnbk2bmyzshacjm2g1kd4zzv2y2az14bw3sjccq5qkpmsfvn9nn"))

(define rust-rustc-demangle-0.1.24
  (crate-source "rustc-demangle" "0.1.24"
                "07zysaafgrkzy2rjgwqdj2a8qdpsm6zv6f5pgpk9x0lm40z9b6vi"))

(define rust-rustc-demangle-0.1.26
  (crate-source "rustc-demangle" "0.1.26"
                "1kja3nb0yhlm4j2p1hl8d7sjmn2g9fa1s4pj0qma5kj2lcndkxsn"))

(define rust-rustc-demangle-0.1.27
  (crate-source "rustc-demangle" "0.1.27"
                "17f0jl6lgsy8kwxdzxp3s2wmipvlpna03kkc4vkqr1gwv5lqh2xm"))

(define rust-rustc-hash-1.1.0
  (crate-source "rustc-hash" "1.1.0"
                "1qkc5khrmv5pqi5l5ca9p5nl5hs742cagrndhbrlk3dhlrx3zm08"))

(define rust-rustc-hash-2.0.0
  (crate-source "rustc-hash" "2.0.0"
                "0lni0lf846bzrf3jvci6jaf4142n1mdqxvcpczk5ch9pfgyk8c2q"))

(define rust-rustc-hash-2.1.0
  (crate-source "rustc-hash" "2.1.0"
                "15yln6fmqlbg0k35r748h8g9xsd637ri23xihq81jb03ncwq1yy7"))

(define rust-rustc-hash-2.1.1
  (crate-source "rustc-hash" "2.1.1"
                "03gz5lvd9ghcwsal022cgkq67dmimcgdjghfb5yb5d352ga06xrm"))

(define rust-rustc-version-0.4.0
  (crate-source "rustc_version" "0.4.0"
                "0rpk9rcdk405xhbmgclsh4pai0svn49x35aggl4nhbkd4a2zb85z"))

(define rust-rustc-version-0.4.1
  (crate-source "rustc_version" "0.4.1"
                "14lvdsmr5si5qbqzrajgb6vfn69k0sfygrvfvr2mps26xwi3mjyg"))

(define rust-rusticata-macros-4.1.0
  (crate-source "rusticata-macros" "4.1.0"
                "0ch67lljmgl5pfrlb90bl5kkp2x6yby1qaxnpnd0p5g9xjkc9w7s"))

(define rust-rustix-0.37.27
  (crate-source "rustix" "0.37.27"
                "1lidfswa8wbg358yrrkhfvsw0hzlvl540g4lwqszw09sg8vcma7y"))

(define rust-rustix-0.38.10
  (crate-source "rustix" "0.38.10"
                "0r096k86c9sbvirfnz5vy8k8qphkkwahcvi6irqfn9d6rbhlhqpd"))

(define rust-rustix-0.38.28
  (crate-source "rustix" "0.38.28"
                "05m3vacvbqbg6r6ksmx9k5afpi0lppjdv712crrpsrfax2jp5rbj"))

(define rust-rustix-0.38.34
  (crate-source "rustix" "0.38.34"
                "03vkqa2ism7q56rkifyy8mns0wwqrk70f4i4fd53r97p8b05xp3h"))

(define rust-rustix-0.38.42
  (crate-source "rustix" "0.38.42"
                "11fvprv3p450ggyqacp7sdpjbbsgm5zvfjwnzy8bfbmbrf7c6ggr"))

(define rust-rustix-0.38.43
  (crate-source "rustix" "0.38.43"
                "1xjfhdnmqsbwnfmm77vyh7ldhqx0g9waqm4982404d7jdgp93257"))

(define rust-rustix-0.38.44
  (crate-source "rustix" "0.38.44"
                "0m61v0h15lf5rrnbjhcb9306bgqrhskrqv7i1n0939dsw8dbrdgx"))

(define rust-rustix-1.0.5
  (crate-source "rustix" "1.0.5"
                "1gsqrw9cp762ps9dl1d13n8mk5r0b6r2s002l1njxfylilwify6r"))

(define rust-rustix-1.0.7
  (crate-source "rustix" "1.0.7"
                "0rhjh16bnxi86nrn9qwcnw5632mvd5m1vdy61s4n9zz7mzb867n7"))

(define rust-rustix-1.0.8
  (crate-source "rustix" "1.0.8"
                "1j6ajqi61agdnh1avr4bplrsgydjw1n4mycdxw3v8g94pyx1y60i"))

(define rust-rustix-1.1.2
  (crate-source "rustix" "1.1.2"
                "0gpz343xfzx16x82s1x336n0kr49j02cvhgxdvaq86jmqnigh5fd"))

(define rust-rustix-1.1.3
  (crate-source "rustix" "1.1.3"
                "0d0z2zcw4rwzni1hm8snw8xdxwwrij336m31c4ghq66cghj9wv0l"))

(define rust-rustls-0.21.12
  (crate-source "rustls" "0.21.12"
                "0gjdg2a9r81sdwkyw3n5yfbkrr6p9gyk3xr2kcsr3cs83x6s2miz"))

(define rust-rustls-0.22.4
  (crate-source "rustls" "0.22.4"
                "0cl4q6w0x1cl5ldjsgbbiiqhkz6qg5vxl5dkn9wwsyxc44vzfkmz"))

(define rust-rustls-0.23.20
  (crate-source "rustls" "0.23.20"
                "06rvj13ia4hx0kba454vcm3p4f2jz907rrabi76k5lyba3rc6rah"))

(define rust-rustls-0.23.21
  (crate-source "rustls" "0.23.21"
                "1f75gicypi40bdq20h1rglwaapr3ifnchgf697clkxibc0j7ja4g"))

(define rust-rustls-0.23.23
  (crate-source "rustls" "0.23.23"
                "15gk2bmry78cps3ya38a7cn4jxc36xv1r7gndr0fbz40qjc6qya7"))

(define rust-rustls-0.23.32
  (crate-source "rustls" "0.23.32"
                "0h2ddlnbjhs47hcmf3rbvr32sxj5kpf0m56rgk739l192rijag6d"))

(define rust-rustls-0.23.34
  (crate-source "rustls" "0.23.34"
                "19vzmdybp5rlgr0bjb4fykp28w2d6fkqq150aamqykrbxvlqd5ba"))

(define rust-rustls-0.23.36
  (crate-source "rustls" "0.23.36"
                "06w0077ssk3blpp93613lkny046mwj0nhxjgc7cmg9nf70yz6rf6"))

(define rust-rustls-0.23.7
  (crate-source "rustls" "0.23.7"
                "0yv7bh16rwhn6fnlb3wnixb2y4in9gf3z3ysa8k3zbgh3nbdpfzb"))

(define rust-rustls-native-certs-0.7.0
  (crate-source "rustls-native-certs" "0.7.0"
                "14ip15dcr6fmjzi12lla9cpln7mmkdid4a7wsp344v4kz9gbh7wg"))

(define rust-rustls-native-certs-0.7.3
  (crate-source "rustls-native-certs" "0.7.3"
                "1r9ib5gwkfci2wbqnbh44nigvrfgxs4n1x89js82w97dxsab7gz5"))

(define rust-rustls-native-certs-0.8.1
  (crate-source "rustls-native-certs" "0.8.1"
                "1ls7laa3748mkn23fmi3g4mlwk131lx6chq2lyc8v2mmabfz5kvz"))

(define rust-rustls-native-certs-0.8.3
  (crate-source "rustls-native-certs" "0.8.3"
                "0qrajg2n90bcr3bcq6j95gjm7a9lirfkkdmjj32419dyyzan0931"))

(define rust-rustls-pemfile-1.0.4
  (crate-source "rustls-pemfile" "1.0.4"
                "1324n5bcns0rnw6vywr5agff3rwfvzphi7rmbyzwnv6glkhclx0w"))

(define rust-rustls-pemfile-2.0.0
  (crate-source "rustls-pemfile" "2.0.0"
                "1x34xidvzn4br2vl8f8xwmhgbjv4lmlb0ggv5whlnk4yl87rir1m"))

(define rust-rustls-pemfile-2.1.2
  (crate-source "rustls-pemfile" "2.1.2"
                "0ggpmk5n7p096nim2hn57facx6rwf76l55qqsj4fny37d0jkm699"))

(define rust-rustls-pemfile-2.2.0
  (crate-source "rustls-pemfile" "2.2.0"
                "0l3f3mrfkgdjrava7ibwzgwc4h3dljw3pdkbsi9rkwz3zvji9qyw"))

(define rust-rustls-pki-types-1.10.1
  (crate-source "rustls-pki-types" "1.10.1"
                "0dqb3d0cbld1yrp084wyzgw6yk3qzzic8l5pbs1b6bcjzzk4ggyj"))

(define rust-rustls-pki-types-1.11.0
  (crate-source "rustls-pki-types" "1.11.0"
                "0755isc0x5iymm3wsn59s0ad1pm9zidw7p34qfqlsjsac9jf4z4i"))

(define rust-rustls-pki-types-1.12.0
  (crate-source "rustls-pki-types" "1.12.0"
                "0yawbdpix8jif6s8zj1p2hbyb7y3bj66fhx0y7hyf4qh4964m6i2"))

(define rust-rustls-pki-types-1.13.0
  (crate-source "rustls-pki-types" "1.13.0"
                "0yjzsnpv1sjbnfxbbmrnyimd23jip48nav6l9hr1rjd06vcjl64l"))

(define rust-rustls-pki-types-1.14.0
  (crate-source "rustls-pki-types" "1.14.0"
                "1p9zsgslvwzzkzhm6bqicffqndr4jpx67992b0vl0pi21a5hy15y"))

(define rust-rustls-pki-types-1.4.1
  (crate-source "rustls-pki-types" "1.4.1"
                "0izj2sa55iwziwsckxfqc66il5s3df616jjw6ca4agly4p26rlzc"))

(define rust-rustls-pki-types-1.7.0
  (crate-source "rustls-pki-types" "1.7.0"
                "0banlc9xzwqrx8n0h4bd0igmq3z5hc72rn941lf22cp3gkkraqlp"))

(define rust-rustls-platform-verifier-0.3.4
  (crate-source "rustls-platform-verifier" "0.3.4"
                "145lhjn9w5khp0fn4lagaa8qqyjyhfqn2dg6llva6qyzvy5qgfxg"))

(define rust-rustls-platform-verifier-0.4.0
  (crate-source "rustls-platform-verifier" "0.4.0"
                "0dnij2d9029i06x4bhyzj5izwv3370233czaqkk1fmgc1wjdrix4"))

(define rust-rustls-platform-verifier-0.6.2
  (crate-source "rustls-platform-verifier" "0.6.2"
                "110pqkn3px9115pb6h6a23cq738v29gbp559dfvpmbibqzmzx68x"))

(define rust-rustls-platform-verifier-android-0.1.1
  (crate-source "rustls-platform-verifier-android" "0.1.1"
                "13vq6sxsgz9547xm2zbdxiw8x7ad1g8n8ax6xvxsjqszk7q6awgq"))

(define rust-rustls-webpki-0.101.7
  (crate-source "rustls-webpki" "0.101.7"
                "0rapfhpkqp75552i8r0y7f4vq7csb4k7gjjans0df73sxv8paqlb"))

(define rust-rustls-webpki-0.102.3
  (crate-source "rustls-webpki" "0.102.3"
                "1gzahmw0lqhszsczmk63bamsf5ps6wa5lsff6gjvqhfxq20ybg7k"))

(define rust-rustls-webpki-0.102.4
  (crate-source "rustls-webpki" "0.102.4"
                "0gmk2abk7y2cdppqlaqmnhcv690p19af9n66sjvw84z9j9z8yi7z"))

(define rust-rustls-webpki-0.102.8
  (crate-source "rustls-webpki" "0.102.8"
                "1sdy8ks86b7jpabpnb2px2s7f1sq8v0nqf6fnlvwzm4vfk41pjk4"))

(define rust-rustls-webpki-0.103.7
  (crate-source "rustls-webpki" "0.103.7"
                "1gqlsd0yqiqch97g0wbsnbmyrp75j6nbzfpf8dmhxa78j50ky2z1"))

(define rust-rustls-webpki-0.103.8
  (crate-source "rustls-webpki" "0.103.8"
                "0lpymb84bi5d2pm017n39nbiaa5cd046hgz06ir29ql6a8pzmz9g"))

(define rust-rustls-webpki-0.103.9
  (crate-source "rustls-webpki" "0.103.9"
                "0lwg1nnyv7pp2lfwwjhy81bxm233am99jnsp3iymdhd6k8827pyp"))

(define rust-rustrict-0.7.33
  (crate-source "rustrict" "0.7.33"
                "051r36qim1f2amj99fn69dpz17jq8zcr2w6cym7nb1ixbsvbwrm5"))

(define rust-rustrict-0.7.36
  (crate-source "rustrict" "0.7.36"
                "0ga9r9wgil0y0clnisja66z2d0nq9vrkdzk2mszgjlvamawijdyr"))

(define rust-rustversion-1.0.14
  (crate-source "rustversion" "1.0.14"
                "1x1pz1yynk5xzzrazk2svmidj69jhz89dz5vrc28sixl20x1iz3z"))

(define rust-rustversion-1.0.17
  (crate-source "rustversion" "1.0.17"
                "1mm3fckyvb0l2209in1n2k05sws5d9mpkszbnwhq3pkq8apjhpcm"))

(define rust-rustversion-1.0.19
  (crate-source "rustversion" "1.0.19"
                "1m39qd65jcd1xgqzdm3017ppimiggh2446xngwp1ngr8hjbmpi7p"))

(define rust-rustversion-1.0.20
  (crate-source "rustversion" "1.0.20"
                "1lhwjb16dsm8brd18bn2bh0ryzc7qi29bi2jjsc6ny2zbwn3ivgd"))

(define rust-rustversion-1.0.22
  (crate-source "rustversion" "1.0.22"
                "0vfl70jhv72scd9rfqgr2n11m5i9l1acnk684m2w83w0zbqdx75k"))

(define rust-rusty-fork-0.3.0
  (crate-source "rusty-fork" "0.3.0"
                "0kxwq5c480gg6q0j3bg4zzyfh2kwmc3v2ba94jw8ncjc8mpcqgfb"))

(define rust-rustybuzz-0.14.1
  (crate-source "rustybuzz" "0.14.1"
                "0p5mr2nk29448qf8rhq0lnmnpdcqwd3v4zmwlgj24wkpfy4czffg"))

(define rust-rustybuzz-0.20.1
  (crate-source "rustybuzz" "0.20.1"
                "00hp1gwykjfli258zs7lqg8p2zdh94dv2mw8zx7f73m0z2b7qg7x"))

(define rust-ryu-1.0.13
  (crate-source "ryu" "1.0.13"
                "0hchlxvjmsz51l06c7r8zwj45pm8bhc3x3czcih27rkx8v03j4zr"))

(define rust-ryu-1.0.16
  (crate-source "ryu" "1.0.16"
                "0k7b90xr48ag5bzmfjp82rljasw2fx28xr3bg1lrpx7b5sljm3gr"))

(define rust-ryu-1.0.18
  (crate-source "ryu" "1.0.18"
                "17xx2s8j1lln7iackzd9p0sv546vjq71i779gphjq923vjh5pjzk"))

(define rust-ryu-1.0.19
  (crate-source "ryu" "1.0.19"
                "1pg6a0b80m32ahygsdkwzs3bfydk4snw695akz4rqxj4lv8a58bf"))

(define rust-ryu-1.0.20
  (crate-source "ryu" "1.0.20"
                "07s855l8sb333h6bpn24pka5sp7hjk2w667xy6a0khkf6sqv5lr8"))

(define rust-ryu-1.0.22
  (crate-source "ryu" "1.0.22"
                "1139acr2kd4n8p36bp1n42xrpaphn6dhwklnazh8hpdnfps4q3x5"))

(define rust-same-file-1.0.6
  (crate-source "same-file" "1.0.6"
                "00h5j1w87dmhnvbv9l8bic3y7xxsnjmssvifw2ayvgx9mb1ivz4k"))

(define rust-sapling-renderdag-0.1.0
  (crate-source "sapling-renderdag" "0.1.0"
                "0qbv8k698kiz8rpr63hn0m7g789pbmpmg7blql0hkgc7mffbizzd"))

(define rust-sapling-streampager-0.11.0
  (crate-source "sapling-streampager" "0.11.0"
                "14ns7a8lmsvnn3kpcs3vdfj7f9ya9pfzbwzgh902sghzhkh5l8k7"))

(define rust-scc-2.3.0
  (crate-source "scc" "2.3.0"
                "0h76ggx51jc7baclcdkp0dygm04prcgk8nbrmjsj2vb8h89wkq98"))

(define rust-scc-2.4.0
  (crate-source "scc" "2.4.0"
                "1k2nwz3bysf1s3r5g437vq9xfm9i4sadfzn5c0k8xx7ynx3g1rj6"))

(define rust-scfg-0.3.1
  (crate-source "scfg" "0.3.1"
                "1xfqn2yy75jg0jzwh9x4bxfi575csgrjjym32sf93hhg9nmknf59"))

(define rust-schannel-0.1.21
  (crate-source "schannel" "0.1.21"
                "1cxivcbczzfv4295pqi5s80kr7nhs7xc0i40zf43b7q5qw3gng3i"))

(define rust-schannel-0.1.22
  (crate-source "schannel" "0.1.22"
                "126zy5jb95fc5hvzyjwiq6lc81r08rdcn6affn00ispp9jzk6dqc"))

(define rust-schannel-0.1.23
  (crate-source "schannel" "0.1.23"
                "0d1m156bsjrws6xzzr1wyfyih9i22mb2csb5pc5kmkrvci2ibjgv"))

(define rust-schannel-0.1.27
  (crate-source "schannel" "0.1.27"
                "0gbbhy28v72kd5iina0z2vcdl3vz63mk5idvkzn5r52z6jmfna8z"))

(define rust-schannel-0.1.28
  (crate-source "schannel" "0.1.28"
                "1qb6s5gyxfz2inz753a4z3mc1d266mwvz0c5w7ppd3h44swq27c9"))

(define rust-schemars-0.8.21
  (crate-source "schemars" "0.8.21"
                "14lyx04388wgbilgcm0nl75w6359nw16glswfqv7x2rpi9329h09"))

(define rust-schemars-1.1.0
  (crate-source "schemars" "1.1.0"
                "12bjw76926x47l3fpmm3harq8gyn5i5hr1wpp8v3flz8sirf2n4m"))

(define rust-schemars-1.2.0
  (crate-source "schemars" "1.2.0"
                "1lk7qb6797dmixnb0qfkmlpsa5p2kgwk29s91xvpmia2hw811sal"))

(define rust-schemars-derive-0.8.21
  (crate-source "schemars_derive" "0.8.21"
                "03ncmrkldfmdc9skmlyysx2vqdlyyz91r5mbavw77zwaay4fbvmi"))

(define rust-schemars-derive-1.1.0
  (crate-source "schemars_derive" "1.1.0"
                "0cr6gqpi6m8zzdv1mqqsr7fin046qinl5isk4fyxwy1x0aj5h61h"))

(define rust-scm-record-0.8.0
  (crate-source "scm-record" "0.8.0"
                "15yjqrq78q24znz1sdykgbrmlb5x6yf4ay3i7rsn65paw77acqij"))

(define rust-scoped-tls-1.0.1
  (crate-source "scoped-tls" "1.0.1"
                "15524h04mafihcvfpgxd8f4bgc3k95aclz8grjkg9a0rxcvn9kz1"))

(define rust-scopeguard-1.1.0
  (crate-source "scopeguard" "1.1.0"
                "1kbqm85v43rq92vx7hfiay6pmcga03vrjbbfwqpyj3pwsg3b16nj"))

(define rust-scopeguard-1.2.0
  (crate-source "scopeguard" "1.2.0"
                "0jcz9sd47zlsgcnm1hdw0664krxwb5gczlif4qngj2aif8vky54l"))

(define rust-scroll-0.13.0
  (crate-source "scroll" "0.13.0"
                "1pbs3vxhrxcvj9hbjw4hiijbqlz0lkfxc9351mv34hcb4ka7q9f1"))

(define rust-scroll-derive-0.13.1
  (crate-source "scroll_derive" "0.13.1"
                "0zd6xq7ffz8nbyjyl603dq16h5d8v95ljl89aw86daqk4gkfyxpd"))

(define rust-sct-0.7.1
  (crate-source "sct" "0.7.1"
                "056lmi2xkzdg1dbai6ha3n57s18cbip4pnmpdhyljli3m99n216s"))

(define rust-sctk-adwaita-0.10.1
  (crate-source "sctk-adwaita" "0.10.1"
                "1v14vqp7k39jk7pgaibwc06qq9vcmi82k7zlv3qpfvq52w17y9xn"))

(define rust-sdd-3.0.10
  (crate-source "sdd" "3.0.10"
                "1jj1brjjasx7r3lf6iyhhrpglx47vzr0z1qi1n0fcszjzv5wy3a9"))

(define rust-sdd-3.0.5
  (crate-source "sdd" "3.0.5"
                "1a9sqd9cjnjcdhrzh4zy1la41jhxg6hiw0ck5k2n7grbnwdi53s7"))

(define rust-seahash-4.1.0
  (crate-source "seahash" "4.1.0"
                "0sxsb64np6bvnppjz5hg4rqpnkczhsl8w8kf2a5lr1c08xppn40w"))

(define rust-sec1-0.7.3
  (crate-source "sec1" "0.7.3"
                "1p273j8c87pid6a1iyyc7vxbvifrw55wbxgr0dh3l8vnbxb7msfk"))

(define rust-secret-lib-1.0.0
  (crate-source "secret-lib" "1.0.0"
                "0bwnizb8bpgcqax0i0i7ga3p535dh1pwi6i1xpx584gnvs2v6d5s"))

(define rust-secret-service-4.0.0
  (crate-source "secret-service" "4.0.0"
                "1m5zkmmhg1wv67g4lr6pqjyqg3yrh3b8bgpw1ykf06qqkbcmmlz4"))

(define rust-secular-1.0.1
  (crate-source "secular" "1.0.1"
                "1davw8k29sycm7f4674d4m44jfa7pn812jm3m3mm76srvz63xp63"))

(define rust-security-framework-2.11.0
  (crate-source "security-framework" "2.11.0"
                "1h0ipvv8wi9wvhad0a9w7jpmb189jng4jhfgnp6vl1lps0zp49y6"))

(define rust-security-framework-2.11.1
  (crate-source "security-framework" "2.11.1"
                "00ldclwx78dm61v7wkach9lcx76awlrv0fdgjdwch4dmy12j4yw9"))

(define rust-security-framework-2.8.2
  (crate-source "security-framework" "2.8.2"
                "0m22kb2b6nrkvwprca8i2a93gms6l6cgfa5zcknlz0cda00vwcm3"))

(define rust-security-framework-2.9.2
  (crate-source "security-framework" "2.9.2"
                "1pplxk15s5yxvi2m1sz5xfmjibp96cscdcl432w9jzbk0frlzdh5"))

(define rust-security-framework-3.2.0
  (crate-source "security-framework" "3.2.0"
                "05mkrddi9i18h9p098d0iimqv1xxz0wd8mbgpbvh9jj67x0205r7"))

(define rust-security-framework-3.5.1
  (crate-source "security-framework" "3.5.1"
                "1vz6pf5qjgx8s0hg805hq6qbcqnll6fs63irvrpgcc7qx91p6adk"))

(define rust-security-framework-sys-2.11.0
  ;; TODO: Check bundled sources.
  (crate-source "security-framework-sys" "2.11.0"
                "1mq7ykz3fi0ba55aj4afz24v9qvwdpkbjiirb197f8h5pnxkcy9i"))

(define rust-security-framework-sys-2.13.0
  ;; TODO: Check bundled sources.
  (crate-source "security-framework-sys" "2.13.0"
                "1mbhagj98y2byhjkr353y1nings01pfa9yk0gxmcb0ydd0vzsqqq"))

(define rust-security-framework-sys-2.14.0
  ;; TODO: Check bundled sources.
  (crate-source "security-framework-sys" "2.14.0"
                "0chwn01qrnvs59i5220bymd38iddy4krbnmfnhf4k451aqfj7ns9"))

(define rust-security-framework-sys-2.15.0
  ;; TODO: Check bundled sources.
  (crate-source "security-framework-sys" "2.15.0"
                "1h6mijxnfrwvl1y4dzwn3m877j6dqp9qn3g37i954j5czazhq7yc"))

(define rust-security-framework-sys-2.8.0
  ;; TODO: Check bundled sources.
  (crate-source "security-framework-sys" "2.8.0"
                "1d13ncr0glxic1l7rqdivvi8nbcwscfm2lw8ppy63sbjc0lvpj9i"))

(define rust-security-framework-sys-2.9.1
  ;; TODO: Check bundled sources.
  (crate-source "security-framework-sys" "2.9.1"
                "0yhciwlsy9dh0ps1gw3197kvyqx1bvc4knrhiznhid6kax196cp9"))

(define rust-self-cell-1.1.0
  (crate-source "self_cell" "1.1.0"
                "1gmxk5bvnnimcif7v1jk8ai2azfvh9djki545nd86vsnphjgrzf2"))

(define rust-self-cell-1.2.1
  (crate-source "self_cell" "1.2.1"
                "0czb22p9c35lyy5r2kh82qyclqmc3l2xw7mp454xnzjp8chzihhn"))

(define rust-semver-1.0.17
  (crate-source "semver" "1.0.17"
                "1vf03d99sikkradjj33q1l9h4fqgd1h7darjypic6pnh4qrkdgdy"))

(define rust-semver-1.0.21
  (crate-source "semver" "1.0.21"
                "1c49snqlfcx93xym1cgwx8zcspmyyxm37xa2fyfgjx1vhalxfzmr"))

(define rust-semver-1.0.23
  (crate-source "semver" "1.0.23"
                "12wqpxfflclbq4dv8sa6gchdh92ahhwn4ci1ls22wlby3h57wsb1"))

(define rust-semver-1.0.24
  (crate-source "semver" "1.0.24"
                "1fmvjjkd3f64y5fqr1nakkq371mnwzv09fbz5mbmdxril63ypdiw"))

(define rust-semver-1.0.25
  (crate-source "semver" "1.0.25"
                "00sy306qpi7vfand7dxm2vc76nlc8fkh1rrhdy0qh12v50nzx7gp"))

(define rust-semver-1.0.26
  (crate-source "semver" "1.0.26"
                "1l5q2vb8fjkby657kdyfpvv40x2i2xqq9bg57pxqakfj92fgmrjn"))

(define rust-semver-1.0.27
  (crate-source "semver" "1.0.27"
                "1qmi3akfrnqc2hfkdgcxhld5bv961wbk8my3ascv5068mc5fnryp"))

(define rust-semver-rs-0.2.0
  (crate-source "semver_rs" "0.2.0"
                "11i58yfy3909ksp6njv42bfa0yqwynz581rkpgnbbrbjp6mf354m"))

(define rust-serde-1.0.188
  (crate-source "serde" "1.0.188"
                "17jlqzfhimsk8w37ifjwnm86nwjzawlbgwmwc7nhwdwslv5hz7ng"))

(define rust-serde-1.0.196
  (crate-source "serde" "1.0.196"
                "0civrvhbwwk442xhlkfdkkdn478by486qxmackq6k3501zk2c047"))

(define rust-serde-1.0.203
  (crate-source "serde" "1.0.203"
                "1500ghq198n6py5anvz5qbqagd9h1hq04f4qpsvjzrvix56snlvj"))

(define rust-serde-1.0.204
  (crate-source "serde" "1.0.204"
                "04kwpwqz559xlhxkggmm8rjxqgywy5swam3kscwsicnbw1cgaxmw"))

(define rust-serde-1.0.217
  (crate-source "serde" "1.0.217"
                "0w2ck1p1ajmrv1cf51qf7igjn2nc51r0izzc00fzmmhkvxjl5z02"))

(define rust-serde-1.0.218
  (crate-source "serde" "1.0.218"
                "0q6z4bnrwagnms0bds4886711l6mc68s979i49zd3xnvkg8wkpz8"))

(define rust-serde-1.0.219
  (crate-source "serde" "1.0.219"
                "1dl6nyxnsi82a197sd752128a4avm6mxnscywas1jq30srp2q3jz"))

(define rust-serde-1.0.226
  (crate-source "serde" "1.0.226"
                "1zcm2asp9fiphbp0k96whabw02kiiqgzxhbyz85vc92v088n9jhd"))

(define rust-serde-1.0.228
  (crate-source "serde" "1.0.228"
                "17mf4hhjxv5m90g42wmlbc61hdhlm6j9hwfkpcnd72rpgzm993ls"))

(define rust-serde-bser-0.4.0
  (crate-source "serde_bser" "0.4.0"
                "05w7iyrm8xrinnqh6b7ydxnd707g5dc7avvcw5d5nbp42p64nsx5"))

(define rust-serde-bytes-0.11.15
  (crate-source "serde_bytes" "0.11.15"
                "0sjwczchd9p4ak4m644jpkv4r181zr8yj14fdjll1fq6rc2caz1q"))

(define rust-serde-bytes-0.11.17
  (crate-source "serde_bytes" "0.11.17"
                "15kds0mw19lvm8ydd1qbharh5rz96zir06yn2silqbfy3cigsdw4"))

(define rust-serde-core-1.0.226
  (crate-source "serde_core" "1.0.226"
                "1936x6cpqgyq57nm7qi416dsc4fiq3jv6d7vh74xmfgdk4wscaxs"))

(define rust-serde-core-1.0.228
  (crate-source "serde_core" "1.0.228"
                "1bb7id2xwx8izq50098s5j2sqrrvk31jbbrjqygyan6ask3qbls1"))

(define rust-serde-derive-1.0.188
  (crate-source "serde_derive" "1.0.188"
                "1wjaclvsfxgqnnnykllvb5gffsxynk66x6h4c1ds6anq8b37mjjf"))

(define rust-serde-derive-1.0.196
  (crate-source "serde_derive" "1.0.196"
                "0rybziqrfaxkaxrybkhrps7zv3ibxnjdk0fwais16zayr5h57j1k"))

(define rust-serde-derive-1.0.203
  (crate-source "serde_derive" "1.0.203"
                "1fmmqmfza3mwxb1v80737dj01gznrh8mhgqgylkndx5npq7bq32h"))

(define rust-serde-derive-1.0.204
  (crate-source "serde_derive" "1.0.204"
                "08p25262mbmhsr2cg0508d5b1wvljj956rvpg0v3qgg6gc8pxkg0"))

(define rust-serde-derive-1.0.217
  (crate-source "serde_derive" "1.0.217"
                "180r3rj5gi5s1m23q66cr5wlfgc5jrs6n1mdmql2njnhk37zg6ss"))

(define rust-serde-derive-1.0.218
  (crate-source "serde_derive" "1.0.218"
                "0azqd74xbpb1v5vf6w1fdbgmwp39ljjfj25cib5rgrzlj7hh75gh"))

(define rust-serde-derive-1.0.219
  (crate-source "serde_derive" "1.0.219"
                "001azhjmj7ya52pmfiw4ppxm16nd44y15j2pf5gkcwrcgz7pc0jv"))

(define rust-serde-derive-1.0.226
  (crate-source "serde_derive" "1.0.226"
                "0cyvkilp34an3f90b0idw0jjsyq20h7v47gsp8qkfmrl5zi3mdcd"))

(define rust-serde-derive-1.0.228
  (crate-source "serde_derive" "1.0.228"
                "0y8xm7fvmr2kjcd029g9fijpndh8csv5m20g4bd76w8qschg4h6m"))

(define rust-serde-derive-internals-0.29.0
  (crate-source "serde_derive_internals" "0.29.0"
                "1qsl3pkk9yg57wivsxg8lsw77d94l34z70hclrczx9d3cp7023rk"))

(define rust-serde-derive-internals-0.29.1
  (crate-source "serde_derive_internals" "0.29.1"
                "04g7macx819vbnxhi52cx0nhxi56xlhrybgwybyy7fb9m4h6mlhq"))

(define rust-serde-fmt-1.0.3
  (crate-source "serde_fmt" "1.0.3"
                "190h61yfnzprzb1cvkzmyzcarj52f6xggiz8542xck0h2k5dvm71"))

(define rust-serde-json-1.0.113
  (crate-source "serde_json" "1.0.113"
                "0ycaiff7ar4qx5sy9kvi1kv9rnnfl15kcfmhxiiwknn3n5q1p039"))

(define rust-serde-json-1.0.117
  (crate-source "serde_json" "1.0.117"
                "1hxziifjlc0kn1cci9d4crmjc7qwnfi20lxwyj9lzca2c7m84la5"))

(define rust-serde-json-1.0.122
  (crate-source "serde_json" "1.0.122"
                "1nk8phyvwmhrmcbwdr8rbjd9bqp8np62dcbnhisgymqwjl1n4jvq"))

(define rust-serde-json-1.0.134
  (crate-source "serde_json" "1.0.134"
                "0z8wk61rzpqjmnwhv6k9zikhsfmsb6lr6qbg84aqpr1fqisl23yh"))

(define rust-serde-json-1.0.135
  (crate-source "serde_json" "1.0.135"
                "1n9hcsbxpr2lg5kvlksbcy6bkvqv9rn3hy59600i21kli2i7n39b"))

(define rust-serde-json-1.0.140
  (crate-source "serde_json" "1.0.140"
                "0wwkp4vc20r87081ihj3vpyz5qf7wqkqipq17v99nv6wjrp8n1i0"))

(define rust-serde-json-1.0.142
  (crate-source "serde_json" "1.0.142"
                "19y5mz1npafnd6vlaiv41ns3pb0pv3q9nirdy3bcn3b0havys3q3"))

(define rust-serde-json-1.0.145
  (crate-source "serde_json" "1.0.145"
                "1767y6kxjf7gwpbv8bkhgwc50nhg46mqwm9gy9n122f7v1k6yaj0"))

(define rust-serde-json-1.0.149
  (crate-source "serde_json" "1.0.149"
                "11jdx4vilzrjjd1dpgy67x5lgzr0laplz30dhv75lnf5ffa07z43"))

(define rust-serde-path-to-error-0.1.16
  (crate-source "serde_path_to_error" "0.1.16"
                "19hlz2359l37ifirskpcds7sxg0gzpqvfilibs7whdys0128i6dg"))

(define rust-serde-path-to-error-0.1.20
  (crate-source "serde_path_to_error" "0.1.20"
                "0mxls44p2ycmnxh03zpnlxxygq42w61ws7ir7r0ba6rp5s1gza8h"))

(define rust-serde-repr-0.1.12
  (crate-source "serde_repr" "0.1.12"
                "1aqcsjq5wc51qnflfqzkkrh5ckcmi3c7ypx2amg0i1664088iv5w"))

(define rust-serde-repr-0.1.19
  (crate-source "serde_repr" "0.1.19"
                "1sb4cplc33z86pzlx38234xr141wr3cmviqgssiadisgl8dlar3c"))

(define rust-serde-repr-0.1.20
  (crate-source "serde_repr" "0.1.20"
                "1755gss3f6lwvv23pk7fhnjdkjw7609rcgjlr8vjg6791blf6php"))

(define rust-serde-spanned-0.6.5
  (crate-source "serde_spanned" "0.6.5"
                "1hgh6s3jjwyzhfk3xwb6pnnr1misq9nflwq0f026jafi37s24dpb"))

(define rust-serde-spanned-0.6.8
  (crate-source "serde_spanned" "0.6.8"
                "1q89g70azwi4ybilz5jb8prfpa575165lmrffd49vmcf76qpqq47"))

(define rust-serde-spanned-0.6.9
  (crate-source "serde_spanned" "0.6.9"
                "18vmxq6qfrm110caszxrzibjhy2s54n1g5w1bshxq9kjmz7y0hdz"))

(define rust-serde-spanned-1.0.0
  (crate-source "serde_spanned" "1.0.0"
                "10rv91337k8x8zmfir4h8aiwmwgkq07gdv7h0jxhcwwgk10lqws0"))

(define rust-serde-spanned-1.0.2
  (crate-source "serde_spanned" "1.0.2"
                "1vh4kcnzhw0fbr1jhg41p8yybnp5gmpnh171fy25bgn2a8s7h5sl"))

(define rust-serde-spanned-1.0.3
  (crate-source "serde_spanned" "1.0.3"
                "14j32cqcs6jjdl1c111lz6s0hr913dnmy2kpfd75k2761ym4ahz2"))

(define rust-serde-test-1.0.177
  (crate-source "serde_test" "1.0.177"
                "1vgisk4dgwdmz4prc2iwq8lddrp4pbqdbljk0h3b7dnafgjix43z"))

(define rust-serde-toml-merge-0.3.8
  (crate-source "serde-toml-merge" "0.3.8"
                "0s1rbvgwff7x9fl9034s0v5rfjadifdwljc6wkjqkydzsqay9d4k"))

(define rust-serde-urlencoded-0.7.1
  (crate-source "serde_urlencoded" "0.7.1"
                "1zgklbdaysj3230xivihs30qi5vkhigg323a9m62k8jwf4a1qjfk"))

(define rust-serde-xml-rs-0.6.0
  (crate-source "serde-xml-rs" "0.6.0"
                "10i7dvd0c1clj4jbljd08qs8466nlymx7ma7k3ncksx1rn7affpv"))

(define rust-serde-yaml-0.7.5
  (crate-source "serde_yaml" "0.7.5"
                "0l9hqmzfwswqvx5gci0hji6497gim73r10bjl6ckq9r8vz9rk07g"))

(define rust-serde-yaml-0.9.34+deprecated
  (crate-source "serde_yaml" "0.9.34+deprecated"
                "0isba1fjyg3l6rxk156k600ilzr8fp7crv82rhal0rxz5qd1m2va"))

(define rust-serial-test-3.2.0
  (crate-source "serial_test" "3.2.0"
                "1a8zg87gi28952hzj363ykwd8p1ssrakl1gi3f4xdqa4y84q298v"))

(define rust-serial-test-derive-3.2.0
  (crate-source "serial_test_derive" "3.2.0"
                "1vwyz2k5kiy5jmba0fvp6ph8ia707801bz918n2ff7bm11d2csax"))

(define rust-sha1-0.10.5
  (crate-source "sha1" "0.10.5"
                "18zb80sxn31kxdpl1ly6w17hkrvyf08zbxnpy8ckb6f3h3f96hph"))

(define rust-sha1-0.10.6
  (crate-source "sha1" "0.10.6"
                "1fnnxlfg08xhkmwf2ahv634as30l1i3xhlhkvxflmasi5nd85gz3"))

(define rust-sha1-checked-0.10.0
  (crate-source "sha1-checked" "0.10.0"
                "08s4h1drgwxzfn1mk11rn0r9i0rbjra1m0l2c0fbngij1jn9kxc9"))

(define rust-sha1-smol-1.0.1
  (crate-source "sha1_smol" "1.0.1"
                "0pbh2xjfnzgblws3hims0ib5bphv7r5rfdpizyh51vnzvnribymv"))

(define rust-sha2-0.10.6
  (crate-source "sha2" "0.10.6"
                "1h5xrrv2y06kr1gsz4pwrm3lsp206nm2gjxgbf21wfrfzsavgrl2"))

(define rust-sha2-0.10.7
  (crate-source "sha2" "0.10.7"
                "1n3flx8bjyblmb2n860g8402z7q10caajp2n403n37i3cbcbk7s7"))

(define rust-sha2-0.10.8
  (crate-source "sha2" "0.10.8"
                "1j1x78zk9il95w9iv46dh9wm73r6xrgj32y6lzzw7bxws9dbfgbr"))

(define rust-sha2-0.10.9
  (crate-source "sha2" "0.10.9"
                "10xjj843v31ghsksd9sl9y12qfc48157j1xpb8v1ml39jy0psl57"))

(define rust-sha256-1.6.0
  (crate-source "sha256" "1.6.0"
                "1xn86nlyq9mr4a8ybfw3z3275rnh58mb83phjfbp1sxxca2zr07q"))

(define rust-sha3-0.10.8
  (crate-source "sha3" "0.10.8"
                "0q5s3qlwnk8d5j34jya98j1v2p3009wdmnqdza3yydwgi8kjv1vm"))

(define rust-sharded-slab-0.1.4
  (crate-source "sharded-slab" "0.1.4"
                "0cbb8kgwsyr3zzhsv8jrs3y1j3vsw4jxil42lfq31ikhdy0bl3wh"))

(define rust-sharded-slab-0.1.7
  (crate-source "sharded-slab" "0.1.7"
                "1xipjr4nqsgw34k7a2cgj9zaasl2ds6jwn89886kww93d32a637l"))

(define rust-shell-words-1.1.0
  (crate-source "shell-words" "1.1.0"
                "1plgwx8r0h5ismbbp6cp03740wmzgzhip85k5hxqrrkaddkql614"))

(define rust-shellexpand-3.1.0
  (crate-source "shellexpand" "3.1.0"
                "0jz1i14ziz8gbyj71212s7dqrw6q96f25i48zkmy66fcjhxzl0ys"))

(define rust-shellexpand-utils-0.2.1
  (crate-source "shellexpand-utils" "0.2.1"
                "1x9rwkrkhdaylw9sh2029bpfn5v3pcb61v20pv8ki6sjjcv8ynn1"))

(define rust-shlex-1.3.0
  (crate-source "shlex" "1.3.0"
                "0r1y6bv26c1scpxvhg2cabimrmwgbp4p3wy6syj9n0c4s3q2znhg"))

(define rust-signal-hook-0.3.17
  (crate-source "signal-hook" "0.3.17"
                "0098nsah04spqf3n8niirmfym4wsdgjl57c78kmzijlq8xymh8c6"))

(define rust-signal-hook-0.3.18
  (crate-source "signal-hook" "0.3.18"
                "1qnnbq4g2vixfmlv28i1whkr0hikrf1bsc4xjy2aasj2yina30fq"))

(define rust-signal-hook-mio-0.2.4
  (crate-source "signal-hook-mio" "0.2.4"
                "1k8pl9aafiadr4czsg8zal9b4jdk6kq5985p90i19jc5sh31mnrl"))

(define rust-signal-hook-mio-0.2.5
  (crate-source "signal-hook-mio" "0.2.5"
                "1k20rr76ngvmzr6kskkl7dv8iyb84cbydpjbjk3mpcj0lykijnmp"))

(define rust-signal-hook-registry-1.4.1
  (crate-source "signal-hook-registry" "1.4.1"
                "18crkkw5k82bvcx088xlf5g4n3772m24qhzgfan80nda7d3rn8nq"))

(define rust-signal-hook-registry-1.4.2
  (crate-source "signal-hook-registry" "1.4.2"
                "1cb5akgq8ajnd5spyn587srvs4n26ryq0p78nswffwhv46sf1sd9"))

(define rust-signal-hook-registry-1.4.5
  (crate-source "signal-hook-registry" "1.4.5"
                "042lkqrpnlrgvrrcirgigxyp1zk70d8v0fsr5w7a18k3bw2vh0wj"))

(define rust-signal-hook-registry-1.4.6
  (crate-source "signal-hook-registry" "1.4.6"
                "12y2v1ms5z111fymaw1v8k93m5chnkp21h0jknrydkj8zydp395j"))

(define rust-signal-hook-registry-1.4.7
  (crate-source "signal-hook-registry" "1.4.7"
                "1bgdimrfqcldbplryknv87gywcdj9v29l3nwqbybs5p6p2ca0r3n"))

(define rust-signal-hook-registry-1.4.8
  (crate-source "signal-hook-registry" "1.4.8"
                "06vc7pmnki6lmxar3z31gkyg9cw7py5x9g7px70gy2hil75nkny4"))

(define rust-signature-2.2.0
  (crate-source "signature" "2.2.0"
                "1pi9hd5vqfr3q3k49k37z06p7gs5si0in32qia4mmr1dancr6m3p"))

(define rust-simd-adler32-0.3.7
  (crate-source "simd-adler32" "0.3.7"
                "1zkq40c3iajcnr5936gjp9jjh1lpzhy44p3dq3fiw75iwr1w2vfn"))

(define rust-simd-adler32-0.3.8
  (crate-source "simd-adler32" "0.3.8"
                "18lx2gdgislabbvlgw5q3j5ssrr77v8kmkrxaanp3liimp2sc873"))

(define rust-simd-helpers-0.1.0
  (crate-source "simd_helpers" "0.1.0"
                "19idqicn9k4vhd04ifh2ff41wvna79zphdf2c81rlmpc7f3hz2cm"))

(define rust-simdutf8-0.1.5
  (crate-source "simdutf8" "0.1.5"
                "0vmpf7xaa0dnaikib5jlx6y4dxd3hxqz6l830qb079g7wcsgxag3"))

(define rust-similar-2.6.0
  (crate-source "similar" "2.6.0"
                "0vk89dx2mmjp81pmszsa1s3mpzvbiy4krvfbq3s3mc3k27wd9q8x"))

(define rust-similar-2.7.0
  (crate-source "similar" "2.7.0"
                "1aidids7ymfr96s70232s6962v5g9l4zwhkvcjp4c5hlb6b5vfxv"))

(define rust-simple-logger-4.2.0
  (crate-source "simple_logger" "4.2.0"
                "0cwkcbd8ba73ic8g1n896n7mhrgdlm4hnqgvk6vcj5dq55fcsc12"))

(define rust-simple-logger-5.0.0
  (crate-source "simple_logger" "5.0.0"
                "1swpfkym2f3pajcblskk7h82gmcljyfzs3xa0hvmarw7w2jxzig8"))

(define rust-simplecss-0.2.1
  (crate-source "simplecss" "0.2.1"
                "17g8q1z9xrkd27ic9nrfirj6in4rai6l9ws0kxz45n97573ff6x1"))

(define rust-simplecss-0.2.2
  (crate-source "simplecss" "0.2.2"
                "0v0kid7b2602kcka2x2xs9wwfjf8lnvpgpl8x287qg4wra1ni73s"))

(define rust-simplelog-0.12.2
  (crate-source "simplelog" "0.12.2"
                "1h59cp84gwdmbxiljq6qmqq1x3lv9ikc1gb32f5ya7pgzbdpl98n"))

(define rust-siphasher-0.3.11
  (crate-source "siphasher" "0.3.11"
                "03axamhmwsrmh0psdw3gf7c0zc4fyl5yjxfifz9qfka6yhkqid9q"))

(define rust-siphasher-1.0.1
  (crate-source "siphasher" "1.0.1"
                "17f35782ma3fn6sh21c027kjmd227xyrx06ffi8gw4xzv9yry6an"))

(define rust-skrifa-0.22.3
  (crate-source "skrifa" "0.22.3"
                "1gic72m35lr7qil61lyz8pmakgbx3dqjd0zdxx7dsnvc3ynl874f"))

(define rust-skrifa-0.33.2
  (crate-source "skrifa" "0.33.2"
                "0vaqb9skffagqri5p9xc05c33sjv161zhki231kickv2wmwizk3q"))

(define rust-skrifa-0.37.0
  (crate-source "skrifa" "0.37.0"
                "0hcqlhgbiv5lhjv6l0hfj2gm1nabvdy9iv9zry77acpmxlfhfccc"))

(define rust-slab-0.4.11
  (crate-source "slab" "0.4.11"
                "12bm4s88rblq02jjbi1dw31984w61y2ldn13ifk5gsqgy97f8aks"))

(define rust-slab-0.4.8
  (crate-source "slab" "0.4.8"
                "0bgwxig8gkqp6gz8rvrpdj6qwa10karnsxwx7wsj5ay8kcf3aa35"))

(define rust-slab-0.4.9
  (crate-source "slab" "0.4.9"
                "0rxvsgir0qw5lkycrqgb1cxsvxzjv9bmx73bk5y42svnzfba94lg"))

(define rust-sled-0.34.7
  (crate-source "sled" "0.34.7"
                "0dcr2s7cylj5mb33ci3kpx7fz797jwvysnl5airrir9cgirv95kz"))

(define rust-slotmap-1.0.7
  (crate-source "slotmap" "1.0.7"
                "0amqb2fn9lcy1ri0risblkcp88dl0rnfmynw7lx0nqwza77lmzyv"))

(define rust-slotmap-1.1.1
  (crate-source "slotmap" "1.1.1"
                "0f20xf53zaysx9ydzkwwqm6hsjyb8lj2j6amhg57iln3jcy8rmdx"))

(define rust-smallvec-1.13.1
  (crate-source "smallvec" "1.13.1"
                "1mzk9j117pn3k1gabys0b7nz8cdjsx5xc6q7fwnm8r0an62d7v76"))

(define rust-smallvec-1.13.2
  (crate-source "smallvec" "1.13.2"
                "0rsw5samawl3wsw6glrsb127rx6sh89a8wyikicw6dkdcjd1lpiw"))

(define rust-smallvec-1.14.0
  (crate-source "smallvec" "1.14.0"
                "1z8wpr53x6jisklqhkkvkgyi8s5cn69h2d2alhqfxahzxwiq7kvz"))

(define rust-smallvec-1.15.0
  (crate-source "smallvec" "1.15.0"
                "1sgfw8z729nlxk8k13dhs0a762wnaxmlx70a7xlf3wz989bjh5w9"))

(define rust-smallvec-1.15.1
  (crate-source "smallvec" "1.15.1"
                "00xxdxxpgyq5vjnpljvkmy99xij5rxgh913ii1v16kzynnivgcb7"))

(define rust-smartstring-1.0.1
  (crate-source "smartstring" "1.0.1"
                "0agf4x0jz79r30aqibyfjm1h9hrjdh0harcqcvb2vapv7rijrdrz"))

(define rust-smawk-0.3.2
  (crate-source "smawk" "0.3.2"
                "0344z1la39incggwn6nl45k8cbw2x10mr5j0qz85cdz9np0qihxp"))

(define rust-smithay-client-toolkit-0.18.1
  (crate-source "smithay-client-toolkit" "0.18.1"
                "0fpm2yfpsif7022w4xalxgn53y8w5dc8zkipcpbj1n1vmgpd6bwj"))

(define rust-smithay-client-toolkit-0.19.2
  (crate-source "smithay-client-toolkit" "0.19.2"
                "05h05hg4dn3v6br5jbdbs5nalk076a64s7fn6i01nqzby2hxwmrl"))

(define rust-smithay-client-toolkit-0.20.0
  (crate-source "smithay-client-toolkit" "0.20.0"
                "1h2cacmsh9zpw6sgmap49zx7cqhksfwas91mm40i5cz2ylwdl4h5"))

(define rust-smithay-clipboard-0.7.2
  (crate-source "smithay-clipboard" "0.7.2"
                "0inqnr4x458nqz0wdcanbgn77dad3bj0m7pjj074lrv3qkp1d0nc"))

(define rust-smithay-clipboard-0.7.3
  (crate-source "smithay-clipboard" "0.7.3"
                "09hjm3dyjq4s3nxfzi65bg95hv540fi5zr5xad879xrryw1lqw3i"))

(define rust-smol-2.0.2
  (crate-source "smol" "2.0.2"
                "17srgkyzswns43cnqcfb1846a8nsna47qj7wrk1rj8c9c3id6fx3"))

(define rust-smol-macros-0.1.1
  (crate-source "smol-macros" "0.1.1"
                "1hmvkxjd0nbv5jqjp9v7c48ms7kvxjaqj4w8i64scx845svfvjmz"))

(define rust-smol-str-0.2.2
  (crate-source "smol_str" "0.2.2"
                "1bfylqf2vnqaglw58930vpxm2rfzji5gjp15a2c0kh8aj6v8ylyx"))

(define rust-smol-str-0.3.2
  (crate-source "smol_str" "0.3.2"
                "039mj6lc1vkljj17ndlzzkak8kvlmw8ppi6yjdxsh433snfbhxln"))

(define rust-smol-str-0.3.4
  (crate-source "smol_str" "0.3.4"
                "1xfbsr2s5c3cb66dq76a9sn744k9l7xarvqfy8119vwkgyib161l"))

(define rust-smtp-proto-0.1.5
  (crate-source "smtp-proto" "0.1.5"
                "0fm5cc87hn52cjyjbkkv9xm9v4d90madcamhpbgd9w47s4ysvf2i"))

(define rust-snafu-0.7.5
  (crate-source "snafu" "0.7.5"
                "1mj2j2gfbf8mm1hr02zrbrqrh2zp01f61xgkx0lpln2w0ankgpp4"))

(define rust-snafu-derive-0.7.5
  (crate-source "snafu-derive" "0.7.5"
                "1gzy9rzggs090zf7hfvgp4lm1glrmg9qzh796686jnq7bxk7j04r"))

(define rust-socket2-0.4.9
  (crate-source "socket2" "0.4.9"
                "0qnn1r41jqj20m0a2nzzjgzndlmpg5maiyjchccaypfqxq8sk934"))

(define rust-socket2-0.5.10
  (crate-source "socket2" "0.5.10"
                "0y067ki5q946w91xlz2sb175pnfazizva6fi3kfp639mxnmpc8z2"))

(define rust-socket2-0.5.5
  (crate-source "socket2" "0.5.5"
                "1sgq315f1njky114ip7wcy83qlphv9qclprfjwvxcpfblmcsqpvv"))

(define rust-socket2-0.5.7
  (crate-source "socket2" "0.5.7"
                "070r941wbq76xpy039an4pyiy3rfj7mp7pvibf1rcri9njq5wc6f"))

(define rust-socket2-0.5.8
  (crate-source "socket2" "0.5.8"
                "1s7vjmb5gzp3iaqi94rh9r63k9cj00kjgbfn7gn60kmnk6fjcw69"))

(define rust-socket2-0.5.9
  (crate-source "socket2" "0.5.9"
                "1vzds1wwwi0a51fn10r98j7cx3ir4shvhykpbk7md2h5h1ydapsg"))

(define rust-socket2-0.6.0
  (crate-source "socket2" "0.6.0"
                "01qqdzfnr0bvdwq6wl56c9c4m2cvbxn43dfpcv8gjx208sph8d93"))

(define rust-socket2-0.6.1
  (crate-source "socket2" "0.6.1"
                "109qn0kjhqi5zds84qyqi5wn72g8azjhmf4b04fkgkrkd48rw4hp"))

(define rust-softbuffer-0.4.6
  (crate-source "softbuffer" "0.4.6"
                "025v65wsfncn1wp571rr9vj3ghfgnb6y06ai0nnjqy9favfiq18q"))

(define rust-spdx-0.10.6
  (crate-source "spdx" "0.10.6"
                "1g0li9pyw46jzh7b4b6l25164sqdp08jvbp1c4wbb19pysx7nca7"))

(define rust-spin-0.9.8
  (crate-source "spin" "0.9.8"
                "0rvam5r0p3a6qhc18scqpvpgb3ckzyqxpgdfyjnghh8ja7byi039"))

(define rust-spirv-0.3.0+sdk-1.3.268.0
  (crate-source "spirv" "0.3.0+sdk-1.3.268.0"
                "0i3qj7yvvprai1s03dvll2gkfy8398nl64wvllkhaaa4vh1i197d"))

(define rust-spirv-cross-sys-0.4.3+e670b39
  ;; TODO: Check bundled sources.
  (crate-source "spirv-cross-sys" "0.4.3+e670b39"
                "02yyrggzcbxq6wk2n7vpqx3ippbhf9jl0i5356qjg8j7xmprd8c5"))

(define rust-spirv-cross2-0.4.6
  (crate-source "spirv-cross2" "0.4.6"
                "0wkrhjx77galvhh3xha38yzaq2yccdfqg4a0459k2npaxdiqn93w"))

(define rust-spirv-cross2-derive-0.1.0
  (crate-source "spirv-cross2-derive" "0.1.0"
                "0r2ajfv93h3kylqw0m9m914iic5bmrmic435spalvrkciql7l08q"))

(define rust-spki-0.7.3
  (crate-source "spki" "0.7.3"
                "17fj8k5fmx4w9mp27l970clrh5qa7r5sjdvbsln987xhb34dc7nr"))

(define rust-splines-4.4.1
  (crate-source "splines" "4.4.1"
                "07hf7lq56jqiar7znwhqgaz5jw5v61zxmljd5gfj3d21y169mwsf"))

(define rust-splines-5.0.0
  (crate-source "splines" "5.0.0"
                "19ysycxw3r2kplmj1jmspw06gkfcxig5pvc0pazmhwmkysdvx0mq"))

(define rust-splitty-1.0.2
  (crate-source "splitty" "1.0.2"
                "0ryxmv3yfd96qdc4zasjmqw2r1j62fpnqq2vclfdgr17d0g0mdrd"))

(define rust-sqlite-0.32.0
  (crate-source "sqlite" "0.32.0"
                "1rpqpkpxn2qdvghsnak2b73cn5ca37p6ri0ylyjdcmrq3481r003"))

(define rust-sqlite-0.37.0
  (crate-source "sqlite" "0.37.0"
                "10s7kkypkvnx5dkxdi1vaj8vby015irvmnqh757iadhrl40rqvpn"))

(define rust-sqlite3-src-0.5.1
  ;; TODO: Check bundled sources.
  (crate-source "sqlite3-src" "0.5.1"
                "0m74wrkpify3z0xvrw4i2yssn9m9sjwqa5ipk6aq6f7fl58mmjdz"))

(define rust-sqlite3-src-0.7.0
  ;; TODO: Check bundled sources.
  (crate-source "sqlite3-src" "0.7.0"
                "081i23jsrmzna0j2q63sb4ipz8pllnb1fhlywqrhlsw8c34d7dp5"))

(define rust-sqlite3-sys-0.15.2
  ;; TODO: Check bundled sources.
  (crate-source "sqlite3-sys" "0.15.2"
                "0fq6m21dnd5yqrzknsmnl2565nahdwa29s7x12xhxr1kjik2qxgj"))

(define rust-sqlite3-sys-0.18.0
  ;; TODO: Check bundled sources.
  (crate-source "sqlite3-sys" "0.18.0"
                "0fbh4rjq4kc1mx6rigap6xzl5n7skbi9wyhjh581sfn1mnbisy57"))

(define rust-stable-deref-trait-1.2.0
  (crate-source "stable_deref_trait" "1.2.0"
                "1lxjr8q2n534b2lhkxd6l6wcddzjvnksi58zv11f9y0jjmr15wd8"))

(define rust-stable-deref-trait-1.2.1
  (crate-source "stable_deref_trait" "1.2.1"
                "15h5h73ppqyhdhx6ywxfj88azmrpml9gl6zp3pwy2malqa6vxqkc"))

(define rust-static-assertions-1.1.0
  (crate-source "static_assertions" "1.1.0"
                "0gsl6xmw10gvn3zs1rv99laj5ig7ylffnh71f9l34js4nr4r7sx2"))

(define rust-str-buf-1.0.6
  (crate-source "str-buf" "1.0.6"
                "1l7q4nha7wpsr0970bfqm773vhmpwr9l6rr8r4gwgrh46wvdh24y"))

(define rust-strict-0.2.0
  (crate-source "strict" "0.2.0"
                "01j0h28xzg07kd1km5m0wz88asp6hwh45n8q8bdkjymqlpz4897l"))

(define rust-strict-num-0.1.1
  (crate-source "strict-num" "0.1.1"
                "0cb7l1vhb8zj90mzm8avlk815k40sql9515s865rqdrdfavvldv6"))

(define rust-strip-ansi-escapes-0.2.0
  (crate-source "strip-ansi-escapes" "0.2.0"
                "1ymwcax1vyacqxx5xisfsynm7n1bvmhskvsaylac915k8gwqxzsm"))

(define rust-strip-ansi-escapes-0.2.1
  (crate-source "strip-ansi-escapes" "0.2.1"
                "0980min1s9f5g47rwlq8l9njks952a0jlz0v7yxrm5p7www813ra"))

(define rust-strsim-0.10.0
  (crate-source "strsim" "0.10.0"
                "08s69r4rcrahwnickvi0kq49z524ci50capybln83mg6b473qivk"))

(define rust-strsim-0.11.0
  (crate-source "strsim" "0.11.0"
                "00gsdp2x1gkkxsbjxgrjyil2hsbdg49bwv8q2y1f406dwk4p7q2y"))

(define rust-strsim-0.11.1
  (crate-source "strsim" "0.11.1"
                "0kzvqlw8hxqb7y598w1s0hxlnmi84sg5vsipp3yg5na5d1rvba3x"))

(define rust-strsim-0.9.3
  (crate-source "strsim" "0.9.3"
                "0k497pv882qn3q977ckznm13vxx927g8s1swvcv68j3c1pccwik4"))

(define rust-strum-0.25.0
  (crate-source "strum" "0.25.0"
                "09g1q55ms8vax1z0mxlbva3vm8n2r1179kfvbccnkjcidzm58399"))

(define rust-strum-0.26.3
  (crate-source "strum" "0.26.3"
                "01lgl6jvrf4j28v5kmx9bp480ygf1nhvac8b4p7rcj9hxw50zv4g"))

(define rust-strum-0.27.2
  (crate-source "strum" "0.27.2"
                "1ksb9jssw4bg9kmv9nlgp2jqa4vnsa3y4q9zkppvl952q7vdc8xg"))

(define rust-strum-macros-0.25.3
  (crate-source "strum_macros" "0.25.3"
                "184y62g474zqb2f7n16x3ghvlyjbh50viw32p9w9l5lwmjlizp13"))

(define rust-strum-macros-0.26.4
  (crate-source "strum_macros" "0.26.4"
                "1gl1wmq24b8md527cpyd5bw9rkbqldd7k1h38kf5ajd2ln2ywssc"))

(define rust-strum-macros-0.27.2
  (crate-source "strum_macros" "0.27.2"
                "19xwikxma0yi70fxkcy1yxcv0ica8gf3jnh5gj936jza8lwcx5bn"))

(define rust-subtle-2.5.0
  (crate-source "subtle" "2.5.0"
                "1g2yjs7gffgmdvkkq0wrrh0pxds3q0dv6dhkw9cdpbib656xdkc1"))

(define rust-subtle-2.6.0
  (crate-source "subtle" "2.6.0"
                "1rfff8wvwgr0clpf3h5j866jd8di5jchdsr6xlbxzhx0id00h0hd"))

(define rust-subtle-2.6.1
  (crate-source "subtle" "2.6.1"
                "14ijxaymghbl1p0wql9cib5zlwiina7kall6w7g89csprkgbvhhk"))

(define rust-sval-2.13.2
  (crate-source "sval" "2.13.2"
                "1n5grsgvdvyf3zdg2nji5v2663r4nplzlgr7fc7b57f462c0zp7n"))

(define rust-sval-buffer-2.13.2
  (crate-source "sval_buffer" "2.13.2"
                "0br1xqldssd1drjxms5p5virk273a3bi97ihsy7yzh23mpvj56a2"))

(define rust-sval-dynamic-2.13.2
  (crate-source "sval_dynamic" "2.13.2"
                "1x0gnmrw8r37fdpn5c1xh3rlhqwpn1cvc680lc8nqf9rv3snzwb8"))

(define rust-sval-fmt-2.13.2
  (crate-source "sval_fmt" "2.13.2"
                "155f9fbpx3823q66j4w1yv2ismyv26ffijwwgxav0lb11fl2f760"))

(define rust-sval-json-2.13.2
  (crate-source "sval_json" "2.13.2"
                "0mgiydngv45jppkn4n36kr2rclxhyh661n39h0m2nqnsf0ygdvhd"))

(define rust-sval-nested-2.13.2
  (crate-source "sval_nested" "2.13.2"
                "17vf2pv22bn3df6m9q9gg12z1m4crzvhqaaxqcabizp1dabyb753"))

(define rust-sval-ref-2.13.2
  (crate-source "sval_ref" "2.13.2"
                "1ymixfmmyvy6qpxrljc32zmgya9m0a9a05lkphlag58pfpinwz5v"))

(define rust-sval-serde-2.13.2
  (crate-source "sval_serde" "2.13.2"
                "0ak5zjfk81pf5m26q30qh8di7gvnsnjy6d1n6jd10s0v6b874m9a"))

(define rust-svg-0.16.0
  (crate-source "svg" "0.16.0"
                "10scx3bxv9sv5jam4aar1bbbrmjsp7ins04phzggxmkg69f1qgjq"))

(define rust-svg-fmt-0.4.4
  (crate-source "svg_fmt" "0.4.4"
                "1am0f2a9q4qw8m1m85ac45qdi7x7wh2504s22yy4qb6qf4yq2pff"))

(define rust-svg-fmt-0.4.5
  (crate-source "svg_fmt" "0.4.5"
                "1yq2kv7klg8s5k19prn36cprlnb811cqgwhy079g7lng651wr4q1"))

(define rust-svgtypes-0.15.2
  (crate-source "svgtypes" "0.15.2"
                "03h0raj2lpd6hdc47r37q0w3w2zlcm5418xn1bnvxalfqhyfakbr"))

(define rust-svgtypes-0.15.3
  (crate-source "svgtypes" "0.15.3"
                "1z4a0b76ww6rf2c8zdapqh2a7r7kmmy7m957q5h5ics4zwgm9iv8"))

(define rust-swash-0.1.19
  (crate-source "swash" "0.1.19"
                "18ic5i3zzayd4f05k66y1mrrrpgdf21c2n27myajrpcx6lzrzmfb"))

(define rust-swayipc-async-2.0.1
  (crate-source "swayipc-async" "2.0.1"
                "0x2gb68ah524sjfmgmmaph642n3vimvf5zqday3qarcydz6swbl1"))

(define rust-swayipc-async-3.0.0
  (crate-source "swayipc-async" "3.0.0"
                "0rys9dmhskmn5ggm3xk584jr4d69plx11kvvn7g7k6h04x4k8vw0"))

(define rust-swayipc-types-1.3.0
  (crate-source "swayipc-types" "1.3.0"
                "1cynzj51jlvbbhxzyk4gjchamqyh8kwjd5yz9b0cq9fqb503pd24"))

(define rust-swayipc-types-2.0.1
  (crate-source "swayipc-types" "2.0.1"
                "0l560p7jdpbw1zmj9lp8y1hl8pbwvv4vcqkrsqmhb6l1czf6b4d2"))

(define rust-symphonia-0.5.4
  (crate-source "symphonia" "0.5.4"
                "1agmsnmzgsmvd70hq760nvkjrb52nnjmz5hgn1xp6x7fwwm98p41"))

(define rust-symphonia-0.5.5
  (crate-source "symphonia" "0.5.5"
                "0fbhlmvf1m9rb5xdy057vzymvirmzx39gx4hl3x9p7d1630a8wsp"))

(define rust-symphonia-bundle-mp3-0.5.4
  (crate-source "symphonia-bundle-mp3" "0.5.4"
                "1m062zkxq2cbwqxbm3qp4qvgpc9hm49g23vgdc4zpwghf2p2l760"))

(define rust-symphonia-bundle-mp3-0.5.5
  (crate-source "symphonia-bundle-mp3" "0.5.5"
                "1vapgi7haxmi4fnf09rvc4z6q24136m5gsg3k73ymxbbnmmxswj8"))

(define rust-symphonia-core-0.5.4
  (crate-source "symphonia-core" "0.5.4"
                "1hr2w2a217vq4lpghszmsdwxr5ilh5d1ysfm3cixbirxkrvhd0vr"))

(define rust-symphonia-core-0.5.5
  (crate-source "symphonia-core" "0.5.5"
                "1by293wrwb37as89fx8qzr1klvq6l5jw1pbyz1zvpxmpg57wq07a"))

(define rust-symphonia-metadata-0.5.4
  (crate-source "symphonia-metadata" "0.5.4"
                "0g02lhhyf6yyxm7bynx5b9fn2ha39y8fp6cfn72qj05186c2nqmw"))

(define rust-symphonia-metadata-0.5.5
  (crate-source "symphonia-metadata" "0.5.5"
                "05kbkshrzqj83mlbkdwxkgkjzmhb3q99xm4rzid6xzlz5gs6yc1n"))

(define rust-syn-0.11.11
  (crate-source "syn" "0.11.11"
                "1b8x8jdsmj6r9ck7n1pg371526n1q90kx6rv6ivcb22w06wr3f6k"))

(define rust-syn-1.0.109
  (crate-source "syn" "1.0.109"
                "0ds2if4600bd59wsv7jjgfkayfzy3hnazs394kz6zdkmna8l3dkj"))

(define rust-syn-2.0.100
  (crate-source "syn" "2.0.100"
                "18623wdkns03blpv65xsjn8fipl9p9hj98vlrnhin7nqran496mh"))

(define rust-syn-2.0.106
  (crate-source "syn" "2.0.106"
                "19mddxp1ia00hfdzimygqmr1jqdvyl86k48427bkci4d08wc9rzd"))

(define rust-syn-2.0.108
  (crate-source "syn" "2.0.108"
                "05z908svb0yw5wzrlv27l2i8j1d8l16hd5r8bjh809146myr2n6s"))

(define rust-syn-2.0.111
  (crate-source "syn" "2.0.111"
                "11rf9l6435w525vhqmnngcnwsly7x4xx369fmaqvswdbjjicj31r"))

(define rust-syn-2.0.114
  (crate-source "syn" "2.0.114"
                "0akw62dizhyrkf3ym1jsys0gy1nphzgv0y8qkgpi6c1s4vghglfl"))

(define rust-syn-2.0.48
  (crate-source "syn" "2.0.48"
                "0gqgfygmrxmp8q32lia9p294kdd501ybn6kn2h4gqza0irik2d8g"))

(define rust-syn-2.0.67
  (crate-source "syn" "2.0.67"
                "144c0apb6zqqlxdyiiy42ynlahn1ddw66cpxwd7azww63pnmb1pz"))

(define rust-syn-2.0.87
  (crate-source "syn" "2.0.87"
                "0bd3mfcswvn4jkrp7ich5kk58kmpph8412yxd36nsfnh8vilrai5"))

(define rust-syn-2.0.94
  (crate-source "syn" "2.0.94"
                "1wvigzkkcbxhvwyzvyk86iabjh3x835f41mx2q1b3a6y3jzc0ywq"))

(define rust-syn-2.0.96
  (crate-source "syn" "2.0.96"
                "102wk3cgawimi3i0q3r3xw3i858zkyingg6y7gsxfy733amsvl6m"))

(define rust-syn-2.0.98
  (crate-source "syn" "2.0.98"
                "1cfk0qqbl4fbr3dz61nw21d5amvl4rym6nxwnfsw43mf90d7y51n"))

(define rust-sync-wrapper-0.1.2
  (crate-source "sync_wrapper" "0.1.2"
                "0q01lyj0gr9a93n10nxsn8lwbzq97jqd6b768x17c8f7v7gccir0"))

(define rust-sync-wrapper-1.0.1
  (crate-source "sync_wrapper" "1.0.1"
                "150k6lwvr4nl237ngsz8fj5j78k712m4bggrfyjsidllraz5l1m7"))

(define rust-sync-wrapper-1.0.2
  (crate-source "sync_wrapper" "1.0.2"
                "0qvjyasd6w18mjg5xlaq5jgy84jsjfsvmnn12c13gypxbv75dwhb"))

(define rust-synom-0.11.3
  (crate-source "synom" "0.11.3"
                "1dj536sh5xxhan2h0znxhv0sl6sb7lvzmsmrc3nvl3h1v5p0d4x3"))

(define rust-synstructure-0.12.6
  (crate-source "synstructure" "0.12.6"
                "03r1lydbf3japnlpc4wka7y90pmz1i0danaj3f9a7b431akdlszk"))

(define rust-synstructure-0.13.1
  (crate-source "synstructure" "0.13.1"
                "0wc9f002ia2zqcbj0q2id5x6n7g1zjqba7qkg2mr0qvvmdk7dby8"))

(define rust-synstructure-0.13.2
  (crate-source "synstructure" "0.13.2"
                "1lh9lx3r3jb18f8sbj29am5hm9jymvbwh6jb1izsnnxgvgrp12kj"))

(define rust-syntect-no-panic-6.0.0
  (crate-source "syntect-no-panic" "6.0.0"
                "1im47kg8jdc70v4xn43wkiz2kl9mgjjznrgnwanspsaxryx8z96m"))

(define rust-sys-locale-0.3.2
  (crate-source "sys-locale" "0.3.2"
                "1i16hq9mkwpzqvixjfy1ph4i2q5klgagjg4hibz6k894l2crmawf"))

(define rust-syscalls-0.7.0
  (crate-source "syscalls" "0.7.0"
                "1x4wnkz9mf3jacfrxh314nns0igafp3qcna3bmh1j8wnnjsldnwh"))

(define rust-sysinfo-0.29.11
  (crate-source "sysinfo" "0.29.11"
                "0rp6911qqjppvvbh72j27znscrawfvplqlyrj9n0y1n24g27ywnd"))

(define rust-sysinfo-0.36.1
  (crate-source "sysinfo" "0.36.1"
                "0z9141y32amzlg87ky0swsi4myhwngcdpfmjnzzvkrv0a1s00a15"))

(define rust-sysinfo-0.37.2
  (crate-source "sysinfo" "0.37.2"
                "07xizvikp5j2f6jky0j4vlaxp21djznzja1m0z70f77xmxf7sq0n"))

(define rust-system-configuration-0.5.1
  (crate-source "system-configuration" "0.5.1"
                "1rz0r30xn7fiyqay2dvzfy56cvaa3km74hnbz2d72p97bkf3lfms"))

(define rust-system-configuration-0.6.1
  (crate-source "system-configuration" "0.6.1"
                "0sxslml567zm0v8g732314vd2gk9sd3k4xj22xk6p64xir29v1rw"))

(define rust-system-configuration-sys-0.5.0
  ;; TODO: Check bundled sources.
  (crate-source "system-configuration-sys" "0.5.0"
                "1jckxvdr37bay3i9v52izgy52dg690x5xfg3hd394sv2xf4b2px7"))

(define rust-system-configuration-sys-0.6.0
  ;; TODO: Check bundled sources.
  (crate-source "system-configuration-sys" "0.6.0"
                "1i5sqrmgy58l4704hibjbl36hclddglh73fb3wx95jnmrq81n7cf"))

(define rust-system-deps-6.0.4
  (crate-source "system-deps" "6.0.4"
                "0pxh43rd2vr3r3g1kczjkk740ch2s0xbhsx364wny9gng8achpsm"))

(define rust-system-deps-6.2.0
  (crate-source "system-deps" "6.2.0"
                "0c836abhh3k8yn5ymg8wx383ay7n731gkrbbp3gma352yq7mhb9a"))

(define rust-system-deps-6.2.2
  (crate-source "system-deps" "6.2.2"
                "0j93ryw031n3h8b0nfpj5xwh3ify636xmv8kxianvlyyipmkbrd3"))

(define rust-system-deps-7.0.3
  (crate-source "system-deps" "7.0.3"
                "01d0fllzpkfybzadyaq1vlx70imzj56dxs4rk9w2f4ikkypkmlk6"))

(define rust-system-tray-0.2.0
  (crate-source "system-tray" "0.2.0"
                "1q7jf9915r7wsq7mnd9g5b8289bahax659smhvmza4abp2zm7842"))

(define rust-system-tray-0.8.4
  (crate-source "system-tray" "0.8.4"
                "0paqi4al6w8cdv2141qll3brijq6q9akzb7drd1kbwyw0s2b2aqw"))

(define rust-tagptr-0.2.0
  (crate-source "tagptr" "0.2.0"
                "05r4mwvlsclx1ayj65hpzjv3dn4wpi8j4xm695vydccf9k7r683v"))

(define rust-tap-1.0.1
  (crate-source "tap" "1.0.1"
                "0sc3gl4nldqpvyhqi3bbd0l9k7fngrcl4zs47n314nqqk4bpx4sm"))

(define rust-target-lexicon-0.12.13
  (crate-source "target-lexicon" "0.12.13"
                "1bmgpdq766zn61f16py0x9139fv314d054xkrkj9iw3q5vd8nxb9"))

(define rust-target-lexicon-0.12.16
  (crate-source "target-lexicon" "0.12.16"
                "1cg3bnx1gdkdr5hac1hzxy64fhw4g7dqkd0n3dxy5lfngpr1mi31"))

(define rust-temp-dir-0.1.12
  (crate-source "temp-dir" "0.1.12"
                "1mkizff1mxk4d3hng8cmlggrhvic2dr6fxp3dqf05zhmzsgsl5nx"))

(define rust-tempdir-0.3.7
  (crate-source "tempdir" "0.3.7"
                "1n5n86zxpgd85y0mswrp5cfdisizq2rv3la906g6ipyc03xvbwhm"))

(define rust-tempfile-3.10.1
  (crate-source "tempfile" "3.10.1"
                "1wdzz35ri168jn9al4s1g2rnsrr5ci91khgarc2rvpb3nappzdw5"))

(define rust-tempfile-3.14.0
  (crate-source "tempfile" "3.14.0"
                "037f9jm13bmfc6xq9w86dp0nylrddh6ynvl6db4gm1xwzi8y5k18"))

(define rust-tempfile-3.15.0
  (crate-source "tempfile" "3.15.0"
                "016pmkbwn3shas44gcwq1kc9lajalb90qafhiip5fvv8h6f5b2ls"))

(define rust-tempfile-3.17.1
  (crate-source "tempfile" "3.17.1"
                "0c52ggq5vy5mzgk5ly36cgzs1cig3cv6r1jarijmzxgkn6na1r92"))

(define rust-tempfile-3.19.1
  (crate-source "tempfile" "3.19.1"
                "1grmcj8y6rcavndw2dm18ndzdimsq5f8lcrwyg627cdrcdvsqdvl"))

(define rust-tempfile-3.23.0
  (crate-source "tempfile" "3.23.0"
                "05igl2gml6z6i2va1bv49f9f1wb3f752c2i63lvlb9s2vxxwfc9d"))

(define rust-tempfile-3.24.0
  (crate-source "tempfile" "3.24.0"
                "171fz3h6rj676miq15fyv1hnv69p426mlp8489bwa1b3xg3sjpb5"))

(define rust-tempfile-3.5.0
  (crate-source "tempfile" "3.5.0"
                "163rp254r3x7i5hisagrpxid2166pq94jvk511dpkmc1yf2fryxr"))

(define rust-tempfile-3.8.0
  (crate-source "tempfile" "3.8.0"
                "1vsl2193w3gpx3mwj36fwx3v6q2qyvmzrdn6m8fgfsjkrkrx556b"))

(define rust-termbg-0.6.2
  (crate-source "termbg" "0.6.2"
                "1m1x1axm22izpibr3zrmmnr2fzj6fhq7df63zvbxzwxdl5vlbx4b"))

(define rust-termcolor-1.4.1
  (crate-source "termcolor" "1.4.1"
                "0mappjh3fj3p2nmrg4y7qv94rchwi9mzmgmfflr8p2awdj7lyy86"))

(define rust-termimad-0.31.3
  (crate-source "termimad" "0.31.3"
                "1dnbqanrhahx6vy94h74vvpqy7ri2cy0vdvnagr9g74kqk1dj0bk"))

(define rust-termimad-0.34.0
  (crate-source "termimad" "0.34.0"
                "084fi8bc6bi1jd7z9jhj1myna6d1mgiv9kb57fj4wpfn8fh5rzv8"))

(define rust-terminal-clipboard-0.4.1
  (crate-source "terminal-clipboard" "0.4.1"
                "0cd125j4wgdi9fy39vmzmiz93zq9g7gjgsspwq0vai7pbk5xh3sf"))

(define rust-terminal-light-1.8.0
  (crate-source "terminal-light" "1.8.0"
                "17zfshypxkyqb8x4srywnxy8894chmas0ljcfv7a0xfq0vlnpxx6"))

(define rust-terminal-size-0.4.0
  (crate-source "terminal_size" "0.4.0"
                "1vx6a5klj7sjkx59v78gh93j445s09y2fasiykwgsb04rbbrnnag"))

(define rust-terminal-size-0.4.1
  (crate-source "terminal_size" "0.4.1"
                "1sd4nq55h9sjirkx0138zx711ddxq1k1a45lc77ninhzj9zl8ljk"))

(define rust-terminal-size-0.4.3
  (crate-source "terminal_size" "0.4.3"
                "1l7cicmz49c0cyskfp5a389rsai649xi7y032v73475ikjbwpf30"))

(define rust-terminfo-0.9.0
  (crate-source "terminfo" "0.9.0"
                "0qp6rrzkxcg08vjzsim2bw7mid3vi29mizrg70dzbycj0q7q3snl"))

(define rust-termios-0.3.3
  (crate-source "termios" "0.3.3"
                "0sxcs0g00538jqh5xbdqakkzijadr8nj7zmip0c7jz3k83vmn721"))

(define rust-termtree-0.4.1
  (crate-source "termtree" "0.4.1"
                "0xkal5l2r3r9p9j90x35qy4npbdwxz4gskvbijs6msymaangas9k"))

(define rust-termtree-0.5.1
  (crate-source "termtree" "0.5.1"
                "10s610ax6nb70yi7xfmwcb6d3wi9sj5isd0m63gy2pizr2zgwl4g"))

(define rust-termux-clipboard-0.1.0
  (crate-source "termux-clipboard" "0.1.0"
                "0vz3b2c0ra7x5wg6i56247j8z58wkv3dknznjidk34rjr89zyslz"))

(define rust-termwiz-0.23.0
  (crate-source "termwiz" "0.23.0"
                "0925nsy4j35qx0sppr7fixf8wh1pyyp3xc20hv5kf6g859wsycpd"))

(define rust-termwiz-0.23.3
  (crate-source "termwiz" "0.23.3"
                "1xzq6l7rx285ax57dz8gdh44kp1790x0knvfynmimgfc89rb6xj6"))

(define rust-test-case-3.3.1
  (crate-source "test-case" "3.3.1"
                "1a380yzm6787737cw7s09jqmkn9035hghahradl2ikdg2gfm09gb"))

(define rust-test-case-core-3.3.1
  (crate-source "test-case-core" "3.3.1"
                "0krqi0gbi1yyycigyjlak63r8h1n0vms7mg3kckqwlfd87c7zjxd"))

(define rust-test-case-macros-3.3.1
  (crate-source "test-case-macros" "3.3.1"
                "1yvgky3qax73bic6m368q04xc955p4a91mddd6b5fk7d04mfg2aw"))

(define rust-textwrap-0.16.2
  (crate-source "textwrap" "0.16.2"
                "0mrhd8q0dnh5hwbwhiv89c6i41yzmhw4clwa592rrp24b9hlfdf1"))

(define rust-thiserror-1.0.47
  (crate-source "thiserror" "1.0.47"
                "13wdsrdyrq6x3rcydvxlx4mxck0c5v3mz1dj8zp7xhdg63n05a4p"))

(define rust-thiserror-1.0.56
  (crate-source "thiserror" "1.0.56"
                "1b9hnzngjan4d89zjs16i01bcpcnvdwklyh73lj16xk28p37hhym"))

(define rust-thiserror-1.0.58
  (crate-source "thiserror" "1.0.58"
                "15rjgd1abi2mzjgzfhrvmsxf9h65n95h6sp8f4s52q4i00wqhih3"))

(define rust-thiserror-1.0.68
  (crate-source "thiserror" "1.0.68"
                "14m8zrzcgwczrnpd1qpsz3qrspfcw69m4s1rccc9gfqbh3f9kp82"))

(define rust-thiserror-1.0.69
  (crate-source "thiserror" "1.0.69"
                "0lizjay08agcr5hs9yfzzj6axs53a2rgx070a1dsi3jpkcrzbamn"))

(define rust-thiserror-2.0.11
  (crate-source "thiserror" "2.0.11"
                "1z0649rpa8c2smzx129bz4qvxmdihj30r2km6vfpcv9yny2g4lnl"))

(define rust-thiserror-2.0.12
  (crate-source "thiserror" "2.0.12"
                "024791nsc0np63g2pq30cjf9acj38z3jwx9apvvi8qsqmqnqlysn"))

(define rust-thiserror-2.0.15
  (crate-source "thiserror" "2.0.15"
                "0l18636gw537pp3h94jv3nngvy8bbbsbgdmlxj4i762b0qznvmw0"))

(define rust-thiserror-2.0.17
  (crate-source "thiserror" "2.0.17"
                "1j2gixhm2c3s6g96vd0b01v0i0qz1101vfmw0032mdqj1z58fdgn"))

(define rust-thiserror-2.0.9
  (crate-source "thiserror" "2.0.9"
                "1k5j0ri0kjrnlblv5ikaglbkg1sxxwh0qrxbidxgc38rs0zn8wph"))

(define rust-thiserror-impl-1.0.47
  (crate-source "thiserror-impl" "1.0.47"
                "16z1irxb45l011af53diap97x44dixnbp60v9g6pvarrdssj7dkb"))

(define rust-thiserror-impl-1.0.56
  (crate-source "thiserror-impl" "1.0.56"
                "0w9ldp8fa574ilz4dn7y7scpcq66vdjy59qal8qdpwsh7faal3zs"))

(define rust-thiserror-impl-1.0.58
  (crate-source "thiserror-impl" "1.0.58"
                "1xylyqcb8rv5yh2yf97hg4n4kg27qccc0ijafr1zqklrhahkn7y6"))

(define rust-thiserror-impl-1.0.68
  (crate-source "thiserror-impl" "1.0.68"
                "0zjckd8jp6q0db1l8vc85bbcxqzv3sianpx4v29jfkgnlv4ixim7"))

(define rust-thiserror-impl-1.0.69
  (crate-source "thiserror-impl" "1.0.69"
                "1h84fmn2nai41cxbhk6pqf46bxqq1b344v8yz089w1chzi76rvjg"))

(define rust-thiserror-impl-2.0.11
  (crate-source "thiserror-impl" "2.0.11"
                "1hkkn7p2y4cxbffcrprybkj0qy1rl1r6waxmxqvr764axaxc3br6"))

(define rust-thiserror-impl-2.0.12
  (crate-source "thiserror-impl" "2.0.12"
                "07bsn7shydaidvyyrm7jz29vp78vrxr9cr9044rfmn078lmz8z3z"))

(define rust-thiserror-impl-2.0.15
  (crate-source "thiserror-impl" "2.0.15"
                "1q1xc98s70ipx2jymcvi4gwq6sc5k9aw7nbbj3mbd1p96gmrzlj4"))

(define rust-thiserror-impl-2.0.17
  (crate-source "thiserror-impl" "2.0.17"
                "04y92yjwg1a4piwk9nayzjfs07sps8c4vq9jnsfq9qvxrn75rw9z"))

(define rust-thiserror-impl-2.0.9
  (crate-source "thiserror-impl" "2.0.9"
                "1m77z5vb4w7xn7y12zxnbwncva4bwbi45y45xvkf5aki20kzll3v"))

(define rust-thread-local-1.1.7
  (crate-source "thread_local" "1.1.7"
                "0lp19jdgvp5m4l60cgxdnl00yw1hlqy8gcywg9bddwng9h36zp9z"))

(define rust-thread-local-1.1.8
  (crate-source "thread_local" "1.1.8"
                "173i5lyjh011gsimk21np9jn8al18rxsrkjli20a7b8ks2xgk7lb"))

(define rust-thread-local-1.1.9
  (crate-source "thread_local" "1.1.9"
                "1191jvl8d63agnq06pcnarivf63qzgpws5xa33hgc92gjjj4c0pn"))

(define rust-tiff-0.10.3
  (crate-source "tiff" "0.10.3"
                "0vrkdk9cdk07rh7iifcxpn6m8zv3wz695mizhr8rb3gfgzg0b5mg"))

(define rust-tiff-0.9.1
  (crate-source "tiff" "0.9.1"
                "0ghyxlz566dzc3scvgmzys11dhq2ri77kb8sznjakijlxby104xs"))

(define rust-tikv-jemalloc-sys-0.6.1+5.3.0-1-ge13ca993e8ccb9ba9847cc330696e02839f328f7
  ;; TODO: Check bundled sources.
  (crate-source "tikv-jemalloc-sys"
                "0.6.1+5.3.0-1-ge13ca993e8ccb9ba9847cc330696e02839f328f7"
                "0frahmfl05hppiy1sz9g14qb5jv2q4wr323d83xcx8l6mfrab2nd"))

(define rust-tikv-jemallocator-0.6.1
  (crate-source "tikv-jemallocator" "0.6.1"
                "0fh55m13396lii5wj69qh547fq9n2k7r3cczwrkhaklmgwrb8n83"))

(define rust-time-0.3.28
  (crate-source "time" "0.3.28"
                "0j3yl5q4w9vcw55hxxb1a3crls1w82v5dahicj7c4ifjgxavpxhp"))

(define rust-time-0.3.36
  (crate-source "time" "0.3.36"
                "11g8hdpahgrf1wwl2rpsg5nxq3aj7ri6xr672v4qcij6cgjqizax"))

(define rust-time-0.3.37
  (crate-source "time" "0.3.37"
                "08bvydyc14plkwhchzia5bcdbmm0mk5fzilsdpjx06w6hf48drrm"))

(define rust-time-0.3.44
  (crate-source "time" "0.3.44"
                "179awlwb36zly3nmz5h9awai1h4pbf1d83g2pmvlw4v1pgixkrwi"))

(define rust-time-0.3.45
  (crate-source "time" "0.3.45"
                "1gdag88agck220k6fxbgb7gsnr2r14n33sxzm5db9zfp6gy45r7r"))

(define rust-time-core-0.1.1
  (crate-source "time-core" "0.1.1"
                "1yz6d246zbmx9v6wpfg1jyfjlsgagirz7km96pr1mp6snkpzn03k"))

(define rust-time-core-0.1.2
  (crate-source "time-core" "0.1.2"
                "1wx3qizcihw6z151hywfzzyd1y5dl804ydyxci6qm07vbakpr4pg"))

(define rust-time-core-0.1.6
  (crate-source "time-core" "0.1.6"
                "0sqwhg7n47gbffyr0zhipqcnskxgcgzz1ix8wirqs2rg3my8x1j0"))

(define rust-time-core-0.1.7
  (crate-source "time-core" "0.1.7"
                "1jilglvr6m6h2iidnvdp3zfahck9wa7kw64rslk79v1izncfwdlb"))

(define rust-time-macros-0.2.18
  (crate-source "time-macros" "0.2.18"
                "1kqwxvfh2jkpg38fy673d6danh1bhcmmbsmffww3mphgail2l99z"))

(define rust-time-macros-0.2.19
  (crate-source "time-macros" "0.2.19"
                "1pl558z26pp342l5y91n6dxb60xwhar975wk6jc4npiygq0ycd18"))

(define rust-time-macros-0.2.24
  (crate-source "time-macros" "0.2.24"
                "1wzb6hnl35856f58cx259q7ijc4c7yis0qsnydvw5n8jbw9b1krh"))

(define rust-time-macros-0.2.25
  (crate-source "time-macros" "0.2.25"
                "1pg3zrqyvjcy1nh470mdw7qxwwq6zmwq3f1dlp11mxlv4k8m5rbi"))

(define rust-timeago-0.4.2
  (crate-source "timeago" "0.4.2"
                "1rnh92sh1l4jbjvz4g7xvcvmfh7nk5k7mm2w56pnm9z0kmc0wwd1"))

(define rust-timeago-0.5.0
  (crate-source "timeago" "0.5.0"
                "0jaghacg3xw4ix3f7y4mdivp0vny0n48ra8mrrf2klfpxf10f0q5"))

(define rust-tiny-skia-0.11.4
  (crate-source "tiny-skia" "0.11.4"
                "1aq9gd4qh4418g8v08qzakqqggx8hl66qcianl3k5bjdsja37lc3"))

(define rust-tiny-skia-path-0.11.4
  (crate-source "tiny-skia-path" "0.11.4"
                "14ywbdfakvacl6rxxmzbnycplaxpc6i2linh2yqk0sp8qb07z7lw"))

(define rust-tiny-xlib-0.2.4
  (crate-source "tiny-xlib" "0.2.4"
                "17nqhy48ab7vchxzplqrw2g88mx2zyr38kwr1ipan76hxx5m0903"))

(define rust-tinystr-0.7.6
  (crate-source "tinystr" "0.7.6"
                "0bxqaw7z8r2kzngxlzlgvld1r6jbnwyylyvyjbv1q71rvgaga5wi"))

(define rust-tinystr-0.8.1
  (crate-source "tinystr" "0.8.1"
                "12sc6h3hnn6x78iycm5v6wrs2xhxph0ydm43yyn7gdfw8l8nsksx"))

(define rust-tinystr-0.8.2
  (crate-source "tinystr" "0.8.2"
                "0sa8z88axdsf088hgw5p4xcyi6g3w3sgbb6qdp81bph9bk2fkls2"))

(define rust-tinytemplate-1.2.1
  (crate-source "tinytemplate" "1.2.1"
                "1g5n77cqkdh9hy75zdb01adxn45mkh9y40wdr7l68xpz35gnnkdy"))

(define rust-tinyvec-1.10.0
  (crate-source "tinyvec" "1.10.0"
                "1yhk0qdqyiaa4v2j9h8pzax5gxgwpz4da0lcphfil6g6pk1zv9dz"))

(define rust-tinyvec-1.6.0
  (crate-source "tinyvec" "1.6.0"
                "0l6bl2h62a5m44jdnpn7lmj14rd44via8180i7121fvm73mmrk47"))

(define rust-tinyvec-1.8.1
  (crate-source "tinyvec" "1.8.1"
                "1s41rv7n39sjsxz3kd3d4adw45ndkxz1d18rfbz2wd7s9n8bhb82"))

(define rust-tinyvec-1.9.0
  (crate-source "tinyvec" "1.9.0"
                "0w9w8qcifns9lzvlbfwa01y0skhr542anwa3rpn28rg82wgndcq9"))

(define rust-tinyvec-macros-0.1.1
  (crate-source "tinyvec_macros" "0.1.1"
                "081gag86208sc3y6sdkshgw3vysm5d34p431dzw0bshz66ncng0z"))

(define rust-tokio-1.36.0
  (crate-source "tokio" "1.36.0"
                "0c89p36zbd4abr1z3l5mipp43x7z4c9b4vp4s6r8y0gs2mjmya31"))

(define rust-tokio-1.38.0
  (crate-source "tokio" "1.38.0"
                "0jjsb2c9dqi93yij3rqzsh9bk0z3qyasmw1n8qkny3d8lw14lkxs"))

(define rust-tokio-1.39.2
  (crate-source "tokio" "1.39.2"
                "1cb7yhba7nnf00cylcywk7rai5kkdb8b4jzwrc26zgbqqwdzp96s"))

(define rust-tokio-1.41.0
  (crate-source "tokio" "1.41.0"
                "1fwb4nm630hmy9cyl2ar6wxqckgvsakwhg1rhjza4is3a09k8pql"))

(define rust-tokio-1.42.0
  (crate-source "tokio" "1.42.0"
                "0lbmf21h16iibdy7m63ck66grkwa7b1x8yy9gwvp60j5n0hrpv2w"))

(define rust-tokio-1.43.0
  (crate-source "tokio" "1.43.0"
                "17pdm49ihlhfw3rpxix3kdh2ppl1yv7nwp1kxazi5r1xz97zlq9x"))

(define rust-tokio-1.44.2
  (crate-source "tokio" "1.44.2"
                "0j4w3qvlcqzgbxlnap0czvspqj6x461vyk1sbqcf97g4rci8if76"))

(define rust-tokio-1.47.1
  (crate-source "tokio" "1.47.1"
                "0f2hp5v3payg6x04ijj67si1wsdhksskhmjs2k9p5f7bmpyrmr49"))

(define rust-tokio-1.48.0
  (crate-source "tokio" "1.48.0"
                "0244qva5pksy8gam6llf7bd6wbk2vkab9lx26yyf08dix810wdpz"))

(define rust-tokio-1.49.0
  (crate-source "tokio" "1.49.0"
                "11ix3pl03s0bp71q3wddrbf8xr0cpn47d7fzr6m42r3kswy918kj"))

(define rust-tokio-macros-2.2.0
  (crate-source "tokio-macros" "2.2.0"
                "0fwjy4vdx1h9pi4g2nml72wi0fr27b5m954p13ji9anyy8l1x2jv"))

(define rust-tokio-macros-2.3.0
  (crate-source "tokio-macros" "2.3.0"
                "16nkan0x9b62hnqmjqcyd71j1mgpda2sv7gfm2mvbm39l2cfjnjz"))

(define rust-tokio-macros-2.4.0
  (crate-source "tokio-macros" "2.4.0"
                "0lnpg14h1v3fh2jvnc8cz7cjf0m7z1xgkwfpcyy632g829imjgb9"))

(define rust-tokio-macros-2.5.0
  (crate-source "tokio-macros" "2.5.0"
                "1f6az2xbvqp7am417b78d1za8axbvjvxnmkakz9vr8s52czx81kf"))

(define rust-tokio-macros-2.6.0
  (crate-source "tokio-macros" "2.6.0"
                "19czvgliginbzyhhfbmj77wazqn2y8g27y2nirfajdlm41bphh5g"))

(define rust-tokio-native-tls-0.3.1
  (crate-source "tokio-native-tls" "0.3.1"
                "1wkfg6zn85zckmv4im7mv20ca6b1vmlib5xwz9p7g19wjfmpdbmv"))

(define rust-tokio-rustls-0.24.1
  (crate-source "tokio-rustls" "0.24.1"
                "10bhibg57mqir7xjhb2xmf24xgfpx6fzpyw720a4ih8a737jg0y2"))

(define rust-tokio-rustls-0.25.0
  (crate-source "tokio-rustls" "0.25.0"
                "03w6d5aqqf084rmcmrsyq5grhydl53blaiqcl0i2yfnv187hqpkp"))

(define rust-tokio-rustls-0.26.0
  (crate-source "tokio-rustls" "0.26.0"
                "1m00czrmk8x7pdjnz10a3da3i1d0sdf9j9vfp5dnk5ss1q6w8yqc"))

(define rust-tokio-rustls-0.26.1
  (crate-source "tokio-rustls" "0.26.1"
                "0dxz4bhkn4bwnvzjqvqlg70ba5fslnmf9r6yr87wzq5cx9shjvaz"))

(define rust-tokio-rustls-0.26.2
  (crate-source "tokio-rustls" "0.26.2"
                "16wf007q3584j46wc4s0zc4szj6280g23hka6x6bgs50l4v7nwlf"))

(define rust-tokio-rustls-0.26.4
  (crate-source "tokio-rustls" "0.26.4"
                "0qggwknz9w4bbsv1z158hlnpkm97j3w8v31586jipn99byaala8p"))

(define rust-tokio-stream-0.1.14
  (crate-source "tokio-stream" "0.1.14"
                "0hi8hcwavh5sdi1ivc9qc4yvyr32f153c212dpd7sb366y6rhz1r"))

(define rust-tokio-stream-0.1.17
  (crate-source "tokio-stream" "0.1.17"
                "0ix0770hfp4x5rh5bl7vsnr3d4iz4ms43i522xw70xaap9xqv9gc"))

(define rust-tokio-util-0.6.10
  (crate-source "tokio-util" "0.6.10"
                "01v5zkcxjdd5zaniqxxfl6isvd7y5qfmljpqsdyrfrvd3bh3x51n"))

(define rust-tokio-util-0.7.10
  (crate-source "tokio-util" "0.7.10"
                "058y6x4mf0fsqji9rfyb77qbfyc50y4pk2spqgj6xsyr693z66al"))

(define rust-tokio-util-0.7.11
  (crate-source "tokio-util" "0.7.11"
                "1qcz30db6m8lxkl61b3nic4bim1symi636nhbb3rmi3i6xxv9xlw"))

(define rust-tokio-util-0.7.13
  (crate-source "tokio-util" "0.7.13"
                "0y0h10a52c7hrldmr3410bp7j3fadq0jn9nf7awddgd2an6smz6p"))

(define rust-tokio-util-0.7.17
  (crate-source "tokio-util" "0.7.17"
                "152m2rp40bjphca5j581csczarvvr974zvwpzpldcwv0wygi9yif"))

(define rust-tokio-util-0.7.18
  (crate-source "tokio-util" "0.7.18"
                "1600rd47pylwn7cap1k7s5nvdaa9j7w8kqigzp1qy7mh0p4cxscs"))

(define rust-tokio-util-0.7.7
  (crate-source "tokio-util" "0.7.7"
                "1cp6yx4789j6gvbp4xnbk7lpd7q0j2a2qd4g1pg2b4q0afadh9sl"))

(define rust-tokio-vsock-0.7.2
  (crate-source "tokio-vsock" "0.7.2"
                "1izis04x0my0mz8sklf9kp3x04ds8mdhiw5lw6rdm2a877wrwccb"))

(define rust-toml-0.5.11
  (crate-source "toml" "0.5.11"
                "0d2266nx8b3n22c7k24x4428z6di8n83a9n466jm7a2hipfz1xzl"))

(define rust-toml-0.7.6
  (crate-source "toml" "0.7.6"
                "0hk561jxv0zjs8k4i00wwz0skff21gc5gfbsrp51scwwh4x9czn1"))

(define rust-toml-0.8.12
  (crate-source "toml" "0.8.12"
                "18rs8cbzmapwcxf0lpdwz84fmsgp9h5vv6xay5d4m2r0x12ibpg9"))

(define rust-toml-0.8.19
  (crate-source "toml" "0.8.19"
                "0knjd3mkxyb87qcs2dark3qkpadidap3frqfj5nqvhpxwfc1zvd1"))

(define rust-toml-0.8.2
  (crate-source "toml" "0.8.2"
                "0g9ysjaqvm2mv8q85xpqfn7hi710hj24sd56k49wyddvvyq8lp8q"))

(define rust-toml-0.8.20
  (crate-source "toml" "0.8.20"
                "0j012b37iz1mihksr6a928s6dzszxvblzg3l5wxp7azzsv6sb1yd"))

(define rust-toml-0.8.23
  (crate-source "toml" "0.8.23"
                "0qnkrq4lm2sdhp3l6cb6f26i8zbnhqb7mhbmksd550wxdfcyn6yw"))

(define rust-toml-0.9.5
  (crate-source "toml" "0.9.5"
                "1s7n4l40hvpf46jmgidfknnzpyblz4hip7gfkymgn2q0qlfrw4km"))

(define rust-toml-0.9.7
  (crate-source "toml" "0.9.7"
                "187av4nsjc0cdfixpc24sqpxqwy5ijvdm7hd9yfsqx94pzcybr80"))

(define rust-toml-0.9.8
  (crate-source "toml" "0.9.8"
                "1n569s0dgdmqjy21wf85df7kx3vb1zgin3pc2rvy4j8lnqgqpp7h"))

(define rust-toml-datetime-0.6.11
  (crate-source "toml_datetime" "0.6.11"
                "077ix2hb1dcya49hmi1avalwbixmrs75zgzb3b2i7g2gizwdmk92"))

(define rust-toml-datetime-0.6.3
  (crate-source "toml_datetime" "0.6.3"
                "0jsy7v8bdvmzsci6imj8fzgd255fmy5fzp6zsri14yrry7i77nkw"))

(define rust-toml-datetime-0.6.5
  (crate-source "toml_datetime" "0.6.5"
                "1wds4pm2cn6agd38f0ivm65xnc7c7bmk9m0fllcaq82nd3lz8l1m"))

(define rust-toml-datetime-0.6.8
  (crate-source "toml_datetime" "0.6.8"
                "0hgv7v9g35d7y9r2afic58jvlwnf73vgd1mz2k8gihlgrf73bmqd"))

(define rust-toml-datetime-0.7.0
  (crate-source "toml_datetime" "0.7.0"
                "1qwivxqkjxxwcqsvfhxnphpwphci0grdfk197wyxfn1gj0z1rpms"))

(define rust-toml-datetime-0.7.2
  (crate-source "toml_datetime" "0.7.2"
                "1hgff8gdk9yx7dljkqfijmj0sc5ln4xhpj045divdhi7xifhiw9j"))

(define rust-toml-datetime-0.7.3
  (crate-source "toml_datetime" "0.7.3"
                "0cs5f8y4rdsmmwipjclmq97lrwppjy2qa3vja4f9d5xwxcwvdkgj"))

(define rust-toml-datetime-0.7.5+spec-1.1.0
  (crate-source "toml_datetime" "0.7.5+spec-1.1.0"
                "0iqkgvgsxmszpai53dbip7sf2igic39s4dby29dbqf1h9bnwzqcj"))

(define rust-toml-edit-0.19.14
  (crate-source "toml_edit" "0.19.14"
                "12hf91s6mrvjpa0d40j3nibryddyc2szs857zcylm5v9x4kky4pq"))

(define rust-toml-edit-0.19.15
  (crate-source "toml_edit" "0.19.15"
                "08bl7rp5g6jwmfpad9s8jpw8wjrciadpnbaswgywpr9hv9qbfnqv"))

(define rust-toml-edit-0.20.2
  (crate-source "toml_edit" "0.20.2"
                "0f7k5svmxw98fhi28jpcyv7ldr2s3c867pjbji65bdxjpd44svir"))

(define rust-toml-edit-0.22.22
  (crate-source "toml_edit" "0.22.22"
                "1xf7sxfzmnc45f75x302qrn5aph52vc8w226v59yhrm211i8vr2a"))

(define rust-toml-edit-0.22.24
  (crate-source "toml_edit" "0.22.24"
                "0x0lgp70x5cl9nla03xqs5vwwwlrwmd0djkdrp3h3lpdymgpkd0p"))

(define rust-toml-edit-0.22.27
  (crate-source "toml_edit" "0.22.27"
                "16l15xm40404asih8vyjvnka9g0xs9i4hfb6ry3ph9g419k8rzj1"))

(define rust-toml-edit-0.22.9
  (crate-source "toml_edit" "0.22.9"
                "1i3lhd5krd4ah3mgn4m73yla01l7rdlfpl5asy7jb1sikivvnh4f"))

(define rust-toml-edit-0.23.10+spec-1.0.0
  (crate-source "toml_edit" "0.23.10+spec-1.0.0"
                "0saj5c676j8a3sqaj9akkp09wambg8aflji4zblwwa70azvvkj44"))

(define rust-toml-edit-0.23.6
  (crate-source "toml_edit" "0.23.6"
                "0jqq4wz6is0497a42m0wh4j3x4vgp70wrlndd57zzzc61rygxvzk"))

(define rust-toml-edit-0.23.7
  (crate-source "toml_edit" "0.23.7"
                "13cgp4y6prad1lh18bbg64zkq48hafq7xzs4fb0hwpcv1mnyz1b4"))

(define rust-toml-edit-0.23.9
  (crate-source "toml_edit" "0.23.9"
                "0ckqh02lfhacna9m09icwc5fh3d5ihah74shl1bksqs99cxvqz2x"))

(define rust-toml-parser-1.0.2
  (crate-source "toml_parser" "1.0.2"
                "042wp5ni22yqcbrfqq9c63g2vbbp4m59zamxw97hvacs8ipqhldm"))

(define rust-toml-parser-1.0.3
  (crate-source "toml_parser" "1.0.3"
                "09x6i0b57lwc7yn6w1kbd2ypm4vpcrgd2vdax7h745g77g1r7y2c"))

(define rust-toml-parser-1.0.4
  (crate-source "toml_parser" "1.0.4"
                "03l0750d1cyliij9vac4afpp1syh1a6yhbbalnslpnsvsdlf5jy0"))

(define rust-toml-parser-1.0.6+spec-1.1.0
  (crate-source "toml_parser" "1.0.6+spec-1.1.0"
                "0i5zxv5y3z9g6r3gm6ly4q0hhkahh013q4rys2fz04cf195qn6d3"))

(define rust-toml-write-0.1.2
  (crate-source "toml_write" "0.1.2"
                "008qlhqlqvljp1gpp9rn5cqs74gwvdgbvs92wnpq8y3jlz4zi6ax"))

(define rust-toml-writer-1.0.2
  (crate-source "toml_writer" "1.0.2"
                "0r7x3m050c66s9lssaq965vmrsxvxj131db4fq0m5vrd3w4l5j7w"))

(define rust-toml-writer-1.0.3
  (crate-source "toml_writer" "1.0.3"
                "0281l7bgchmlbvxmci01p9x2w5br9p61ylns5ji65rbc24yacqyi"))

(define rust-toml-writer-1.0.4
  (crate-source "toml_writer" "1.0.4"
                "1wkvcdy1ymp2qfipmb74fv3xa7m7qz7ps9hndllasx1nfda2p2yz"))

(define rust-topological-sort-0.2.2
  (crate-source "topological-sort" "0.2.2"
                "0gcxahg24c058izagz642vs0kfb2zja48my3qrd0kkaf2d730s7a"))

(define rust-tower-0.4.13
  (crate-source "tower" "0.4.13"
                "073wncyqav4sak1p755hf6vl66njgfc1z1g1di9rxx3cvvh9pymq"))

(define rust-tower-0.5.1
  (crate-source "tower" "0.5.1"
                "0kvbp97bhb4sk24vhihcz74ngn0i4ygxqikmxndgng3w926r6wr8"))

(define rust-tower-0.5.2
  (crate-source "tower" "0.5.2"
                "1ybmd59nm4abl9bsvy6rx31m4zvzp5rja2slzpn712y9b68ssffh"))

(define rust-tower-0.5.3
  (crate-source "tower" "0.5.3"
                "1m5i3a2z1sgs8nnz1hgfq2nr4clpdmizlp1d9qsg358ma5iyzrgb"))

(define rust-tower-http-0.6.2
  (crate-source "tower-http" "0.6.2"
                "15wnvhl6cpir9125s73bqjzjsvfb0fmndmsimnl2ddnlhfvs6gs0"))

(define rust-tower-http-0.6.6
  (crate-source "tower-http" "0.6.6"
                "1wh51y4rf03f91c6rvli6nwzsarx7097yx6sqlm75ag27pbjzj5d"))

(define rust-tower-http-0.6.8
  (crate-source "tower-http" "0.6.8"
                "1y514jwzbyrmrkbaajpwmss4rg0mak82k16d6588w9ncaffmbrnl"))

(define rust-tower-layer-0.3.2
  (crate-source "tower-layer" "0.3.2"
                "1l7i17k9vlssrdg4s3b0ia5jjkmmxsvv8s9y9ih0jfi8ssz8s362"))

(define rust-tower-layer-0.3.3
  (crate-source "tower-layer" "0.3.3"
                "03kq92fdzxin51w8iqix06dcfgydyvx7yr6izjq0p626v9n2l70j"))

(define rust-tower-lsp-0.20.0
  (crate-source "tower-lsp" "0.20.0"
                "0245266rj84zgd4wv6nsnxcq7nkydqblrhxkv4l7cqm6ahmhbfnl"))

(define rust-tower-lsp-macros-0.9.0
  (crate-source "tower-lsp-macros" "0.9.0"
                "1yi2ahsndbb1xkw05dx95237bq9lq26i0h7ly8klp6hb9qnr1zc4"))

(define rust-tower-service-0.3.2
  (crate-source "tower-service" "0.3.2"
                "0lmfzmmvid2yp2l36mbavhmqgsvzqf7r2wiwz73ml4xmwaf1rg5n"))

(define rust-tower-service-0.3.3
  (crate-source "tower-service" "0.3.3"
                "1hzfkvkci33ra94xjx64vv3pp0sq346w06fpkcdwjcid7zhvdycd"))

(define rust-tracing-0.1.40
  (crate-source "tracing" "0.1.40"
                "1vv48dac9zgj9650pg2b4d0j3w6f3x9gbggf43scq5hrlysklln3"))

(define rust-tracing-0.1.41
  (crate-source "tracing" "0.1.41"
                "1l5xrzyjfyayrwhvhldfnwdyligi1mpqm8mzbi2m1d6y6p2hlkkq"))

(define rust-tracing-0.1.43
  (crate-source "tracing" "0.1.43"
                "0iy6dyqk9ign880xw52snixrs507hj2xqyflaa4kf6aw1c5dj59d"))

(define rust-tracing-0.1.44
  (crate-source "tracing" "0.1.44"
                "006ilqkg1lmfdh3xhg3z762izfwmxcvz0w7m4qx2qajbz9i1drv3"))

(define rust-tracing-appender-0.2.3
  (crate-source "tracing-appender" "0.2.3"
                "1kq69qyjvb4dxch5c9zgii6cqhy9nkk81z0r4pj3y2nc537fhrim"))

(define rust-tracing-appender-0.2.4
  (crate-source "tracing-appender" "0.2.4"
                "1bxf7xvsr89glbq174cx0b9pinaacbhlmc85y1ssniv2rq5lhvbq"))

(define rust-tracing-attributes-0.1.27
  (crate-source "tracing-attributes" "0.1.27"
                "1rvb5dn9z6d0xdj14r403z0af0bbaqhg02hq4jc97g5wds6lqw1l"))

(define rust-tracing-attributes-0.1.28
  (crate-source "tracing-attributes" "0.1.28"
                "0v92l9cxs42rdm4m5hsa8z7ln1xsiw1zc2iil8c6k7lzq0jf2nir"))

(define rust-tracing-attributes-0.1.30
  (crate-source "tracing-attributes" "0.1.30"
                "00v9bhfgfg3v101nmmy7s3vdwadb7ngc8c1iw6wai9vj9sv3lf41"))

(define rust-tracing-attributes-0.1.31
  (crate-source "tracing-attributes" "0.1.31"
                "1np8d77shfvz0n7camx2bsf1qw0zg331lra0hxb4cdwnxjjwz43l"))

(define rust-tracing-chrome-0.7.2
  (crate-source "tracing-chrome" "0.7.2"
                "0977zy46gpawva2laffigxr2pph8v0xa51kfp6ghlifnsn7762mz"))

(define rust-tracing-core-0.1.32
  (crate-source "tracing-core" "0.1.32"
                "0m5aglin3cdwxpvbg6kz0r9r0k31j48n0kcfwsp6l49z26k3svf0"))

(define rust-tracing-core-0.1.33
  (crate-source "tracing-core" "0.1.33"
                "170gc7cxyjx824r9kr17zc9gvzx89ypqfdzq259pr56gg5bwjwp6"))

(define rust-tracing-core-0.1.34
  (crate-source "tracing-core" "0.1.34"
                "0y3nc4mpnr79rzkrcylv5f5bnjjp19lsxwis9l4kzs97ya0jbldr"))

(define rust-tracing-core-0.1.35
  (crate-source "tracing-core" "0.1.35"
                "0v0az9hivci6bysd796za7g823gkasb8qmdqdsiwd2awmd7y413s"))

(define rust-tracing-core-0.1.36
  (crate-source "tracing-core" "0.1.36"
                "16mpbz6p8vd6j7sf925k9k8wzvm9vdfsjbynbmaxxyq6v7wwm5yv"))

(define rust-tracing-error-0.2.0
  (crate-source "tracing-error" "0.2.0"
                "0vi0rns7r8iny2milg0kikz1mhgcl9wz5clpy1vi4jrq1wffr1nn"))

(define rust-tracing-error-0.2.1
  (crate-source "tracing-error" "0.2.1"
                "1nzk6qcvhmxxy3lw1nj71anmfmvxlnk78l5lym1389vs1l1825cb"))

(define rust-tracing-log-0.2.0
  (crate-source "tracing-log" "0.2.0"
                "1hs77z026k730ij1a9dhahzrl0s073gfa2hm5p0fbl0b80gmz1gf"))

(define rust-tracing-subscriber-0.3.18
  (crate-source "tracing-subscriber" "0.3.18"
                "12vs1bwk4kig1l2qqjbbn2nm5amwiqmkcmnznylzmnfvjy6083xd"))

(define rust-tracing-subscriber-0.3.19
  (crate-source "tracing-subscriber" "0.3.19"
                "0220rignck8072i89jjsh140vmh14ydwpdwnifyaf3xcnpn9s678"))

(define rust-tracing-subscriber-0.3.20
  (crate-source "tracing-subscriber" "0.3.20"
                "1m9447bxq7236avgl6n5yb2aqwplrghm61dgipw03mh7ad7s2m10"))

(define rust-tracing-subscriber-0.3.22
  (crate-source "tracing-subscriber" "0.3.22"
                "07hz575a0p1c2i4xw3gs3hkrykhndnkbfhyqdwjhvayx4ww18c1g"))

(define rust-tracing-web-0.1.3
  (crate-source "tracing-web" "0.1.3"
                "0z7yvr0v38yacqhqb1hbawy25jhgymd7izgbj66izmgbzr0s3rmr"))

(define rust-trash-5.2.3
  (crate-source "trash" "5.2.3"
                "1wsgyriw10wl4h46pgs589laq4vwh8q5ma4aflwpl08j212k98v5"))

(define rust-trash-5.2.5
  (crate-source "trash" "5.2.5"
                "1n4lgjyv1vhv8aydwp65w8bfj8k8835w8fhvn675cn7nzha3mfd9"))

(define rust-tree-magic-mini-3.1.6
  (crate-source "tree_magic_mini" "3.1.6"
                "0qwx2b0xfr00vdskl951cvh3m040zj5n8vm7ln4k6p143ybyiida"))

(define rust-tree-sitter-0.22.6
  (crate-source "tree-sitter" "0.22.6"
                "1jkda5n43m7cxmx2h7l20zxc74nf9v1wpm66gvgxrm5drscw8z6z"))

(define rust-tree-sitter-json-0.21.0
  (crate-source "tree-sitter-json" "0.21.0"
                "1nq57g9508fzpzicxwrb05mch4w12pg3spsassvp8pf3fg5pswsv"))

(define rust-triomphe-0.1.11
  (crate-source "triomphe" "0.1.11"
                "1crf71hndy3fc68x8v4aikkdjynp4n5sdhq28sck8x7frx8bd7l5"))

(define rust-try-lock-0.2.4
  (crate-source "try-lock" "0.2.4"
                "1vc15paa4zi06ixsxihwbvfn24d708nsyg1ncgqwcrn42byyqa1m"))

(define rust-try-lock-0.2.5
  (crate-source "try-lock" "0.2.5"
                "0jqijrrvm1pyq34zn1jmy2vihd4jcrjlvsh4alkjahhssjnsn8g4"))

(define rust-ttf-parser-0.20.0
  (crate-source "ttf-parser" "0.20.0"
                "1d4n3p9ccjvy4mj72700i0c2q6d49dxjpwflw47q79rpv1v7vxqp"))

(define rust-ttf-parser-0.21.1
  (crate-source "ttf-parser" "0.21.1"
                "1y0wsfgri7yi41cn57g4fzqm30x1v5nlrci6j5mqcxwpys1isn9c"))

(define rust-ttf-parser-0.25.1
  (crate-source "ttf-parser" "0.25.1"
                "0cbgqglcwwjg3hirwq6xlza54w04mb5x02kf7zx4hrw50xmr1pyj"))

(define rust-tui-big-text-0.8.1
  (crate-source "tui-big-text" "0.8.1"
                "1vrwy9gg2n78pqj3snwyryxzbr3srq5hb50v7i8a97122ivwzcqc"))

(define rust-tui-input-0.15.0
  (crate-source "tui-input" "0.15.0"
                "0i9ci8p1b5nbkypz3fm723va3lzl07kf0saz1w13dwcq8abfxhbr"))

(define rust-tui-popup-0.7.2
  (crate-source "tui-popup" "0.7.2"
                "0d72fjh42dpi4p9krzllly6hnyw2m1b3d9azlygkgih1ww59x6kz"))

(define rust-twofish-0.7.1
  (crate-source "twofish" "0.7.1"
                "04w0ii2c0c9ws08aw6c7illh9zql22il9lbwjk1mgir30aiq73m7"))

(define rust-twox-hash-2.1.1
  (crate-source "twox-hash" "2.1.1"
                "0mnyhzba2qkkl0jk9y9p7l539gx1nghns9fk3ckdbknb8ajpv44b"))

(define rust-twox-hash-2.1.2
  (crate-source "twox-hash" "2.1.2"
                "1721278f1yc5zvkpdb8gsb1x6nlfjdmwm5fk9ff3fismcxmi78wy"))

(define rust-typeid-1.0.2
  (crate-source "typeid" "1.0.2"
                "0vi32jv3s3nbybbl4r317wi2bk8j4fx4d8p88jji8pnd1hpdn4qf"))

(define rust-typeid-1.0.3
  (crate-source "typeid" "1.0.3"
                "0727ypay2p6mlw72gz3yxkqayzdmjckw46sxqpaj08v0b0r64zdw"))

(define rust-typenum-1.16.0
  (crate-source "typenum" "1.16.0"
                "1fhb9iaqyjn4dzn2vl86kxjhp4xpw5gynczlnqzf4x6rjgpn2ya9"))

(define rust-typenum-1.17.0
  (crate-source "typenum" "1.17.0"
                "09dqxv69m9lj9zvv6xw5vxaqx15ps0vxyy5myg33i0kbqvq0pzs2"))

(define rust-typenum-1.18.0
  (crate-source "typenum" "1.18.0"
                "0gwgz8n91pv40gabrr1lzji0b0hsmg0817njpy397bq7rvizzk0x"))

(define rust-typenum-1.19.0
  (crate-source "typenum" "1.19.0"
                "1fw2mpbn2vmqan56j1b3fbpcdg80mz26fm53fs16bq5xcq84hban"))

(define rust-ucd-trie-0.1.5
  (crate-source "ucd-trie" "0.1.5"
                "10ggllapxq99cxxy179wbklmabj5fikm02233v4idf7djvcw8ycy"))

(define rust-ucd-trie-0.1.6
  (crate-source "ucd-trie" "0.1.6"
                "1ff4yfksirqs37ybin9aw71aa5gva00hw7jdxbw8w668zy964r7d"))

(define rust-ucd-trie-0.1.7
  (crate-source "ucd-trie" "0.1.7"
                "0wc9p07sqwz320848i52nvyjvpsxkx3kv5bfbmm6s35809fdk5i8"))

(define rust-udev-0.2.0
  (crate-source "udev" "0.2.0"
                "00gkkicb8vfhf32c4h5lp7z07kw31agivpbx7q9ipsmj94d4sl27"))

(define rust-uds-windows-1.0.2
  (crate-source "uds_windows" "1.0.2"
                "03ckj6vnzvm4r5xd17dxyyqqqcfgs3xqj53hcswykk6k4i1n0rff"))

(define rust-uds-windows-1.1.0
  (crate-source "uds_windows" "1.1.0"
                "1fb4y65pw0rsp0gyfyinjazlzxz1f6zv7j4zmb20l5pxwv1ypnl9"))

(define rust-uluru-3.1.0
  (crate-source "uluru" "3.1.0"
                "1njp6vvy1mm8idnsp6ljyxx5znfsk3xkmk9cr2am0vkfwmlj92kw"))

(define rust-umask-2.1.0
  (crate-source "umask" "2.1.0"
                "071xszsd6znk0ik11pxl7mwhf07clsiq3qpzw1ac0dcyak14d6pc"))

(define rust-unarray-0.1.4
  (crate-source "unarray" "0.1.4"
                "154smf048k84prsdgh09nkm2n0w0336v84jd4zikyn6v6jrqbspa"))

(define rust-unicase-2.8.0
  (crate-source "unicase" "2.8.0"
                "1pznbipizi6yg481167c9mp0xgn1s4cv379pnr9zhmzihf0bclby"))

(define rust-unicase-2.8.1
  (crate-source "unicase" "2.8.1"
                "0fd5ddbhpva7wrln2iah054ar2pc1drqjcll0f493vj3fv8l9f3m"))

(define rust-unicode-bidi-0.3.13
  (crate-source "unicode-bidi" "0.3.13"
                "0q0l7rdkiq54pan7a4ama39dgynaf1mnjj1nddrq1w1zayjqp24j"))

(define rust-unicode-bidi-0.3.15
  (crate-source "unicode-bidi" "0.3.15"
                "0xcdxm7h0ydyprwpcbh436rbs6s6lph7f3gr527lzgv6lw053y88"))

(define rust-unicode-bidi-0.3.18
  (crate-source "unicode-bidi" "0.3.18"
                "1xcxwbsqa24b8vfchhzyyzgj0l6bn51ib5v8j6krha0m77dva72w"))

(define rust-unicode-bidi-mirroring-0.2.0
  (crate-source "unicode-bidi-mirroring" "0.2.0"
                "11mgyp2vclhzcm0kzy8jjn5ivsry4c37368finacb4mwzs7pijr3"))

(define rust-unicode-bidi-mirroring-0.4.0
  (crate-source "unicode-bidi-mirroring" "0.4.0"
                "1zirs1z3ahlwy7swg7apnm3pc6vix1g15q0kn6fx8rmvc266xyjx"))

(define rust-unicode-bom-2.0.3
  (crate-source "unicode-bom" "2.0.3"
                "05s2sqyjanqrbds3fxam35f92npp5ci2wz9zg7v690r0448mvv3y"))

(define rust-unicode-ccc-0.2.0
  (crate-source "unicode-ccc" "0.2.0"
                "0mkn2ns6q2xmmr3g8sxinbhgzkjq9xzavidgipbs6knc3c87pxqx"))

(define rust-unicode-ccc-0.4.0
  (crate-source "unicode-ccc" "0.4.0"
                "0gjhxwx27ywm3rcbb0m5q20w8zxi51440b3ps6swi6ywpj4d8qff"))

(define rust-unicode-general-category-1.1.0
  (crate-source "unicode-general-category" "1.1.0"
                "0zv7q4fdnlawjxd75bpxfll33sf3db09xd13sv85pblkq7fkp68b"))

(define rust-unicode-ident-1.0.11
  (crate-source "unicode-ident" "1.0.11"
                "0g7wmn39nl9yzhjwn9ihacd22ymli8r4nlc2xf3idaas8ypbl6ih"))

(define rust-unicode-ident-1.0.12
  (crate-source "unicode-ident" "1.0.12"
                "0jzf1znfpb2gx8nr8mvmyqs1crnv79l57nxnbiszc7xf7ynbjm1k"))

(define rust-unicode-ident-1.0.14
  (crate-source "unicode-ident" "1.0.14"
                "10ywa1pg0glgkr4l3dppjxizr9r2b7im0ycbfa0137l69z5fdfdd"))

(define rust-unicode-ident-1.0.17
  (crate-source "unicode-ident" "1.0.17"
                "1gpdxvaskz04whays5igg4zyca0dl7vdy2arsfxb13kpjcx4gqh0"))

(define rust-unicode-ident-1.0.18
  (crate-source "unicode-ident" "1.0.18"
                "04k5r6sijkafzljykdq26mhjpmhdx4jwzvn1lh90g9ax9903jpss"))

(define rust-unicode-ident-1.0.19
  (crate-source "unicode-ident" "1.0.19"
                "17bx1j1zf6b9j3kpyf74mraary7ava3984km0n8kh499h5a58fpn"))

(define rust-unicode-ident-1.0.22
  (crate-source "unicode-ident" "1.0.22"
                "1x8xrz17vqi6qmkkcqr8cyf0an76ig7390j9cnqnk47zyv2gf4lk"))

(define rust-unicode-ident-1.0.8
  (crate-source "unicode-ident" "1.0.8"
                "1x4v4v95fv9gn5zbpm23sa9awjvmclap1wh1lmikmw9rna3llip5"))

(define rust-unicode-linebreak-0.1.5
  (crate-source "unicode-linebreak" "0.1.5"
                "07spj2hh3daajg335m4wdav6nfkl0f6c0q72lc37blr97hych29v"))

(define rust-unicode-normalization-0.1.22
  (crate-source "unicode-normalization" "0.1.22"
                "08d95g7b1irc578b2iyhzv4xhsa4pfvwsqxcl9lbcpabzkq16msw"))

(define rust-unicode-normalization-0.1.23
  (crate-source "unicode-normalization" "0.1.23"
                "1x81a50h2zxigj74b9bqjsirxxbyhmis54kg600xj213vf31cvd5"))

(define rust-unicode-normalization-0.1.24
  (crate-source "unicode-normalization" "0.1.24"
                "0mnrk809z3ix1wspcqy97ld5wxdb31f3xz6nsvg5qcv289ycjcsh"))

(define rust-unicode-normalization-0.1.25
  (crate-source "unicode-normalization" "0.1.25"
                "1s76dcrxw7vs32yhpi0p074apdc3s7lak7809f3qvclwij3zdm2z"))

(define rust-unicode-properties-0.1.3
  (crate-source "unicode-properties" "0.1.3"
                "1l3mbgzwz8g14xcs09p4ww3hjkjcf0i1ih13nsg72bhj8n5jl3z7"))

(define rust-unicode-properties-0.1.4
  (crate-source "unicode-properties" "0.1.4"
                "07fpm3sqq7lm9gmgpxa93z31q933h3c3ypfwy4cdh6l42g3miw3x"))

(define rust-unicode-script-0.5.7
  (crate-source "unicode-script" "0.5.7"
                "07vwr9iddw5xwrj57hc6ig0mwmlzjdajj9lyfxqz9by9a2rj3d4z"))

(define rust-unicode-segmentation-1.10.1
  (crate-source "unicode-segmentation" "1.10.1"
                "0dky2hm5k51xy11hc3nk85p533rvghd462b6i0c532b7hl4j9mhx"))

(define rust-unicode-segmentation-1.12.0
  (crate-source "unicode-segmentation" "1.12.0"
                "14qla2jfx74yyb9ds3d2mpwpa4l4lzb9z57c6d2ba511458z5k7n"))

(define rust-unicode-truncate-1.1.0
  (crate-source "unicode-truncate" "1.1.0"
                "1gr7arjjhrhy8dww7hj8qqlws97xf9d276svr4hs6pxgllklcr5k"))

(define rust-unicode-truncate-2.0.1
  (crate-source "unicode-truncate" "2.0.1"
                "19g9af5v0a8xaigqbs9hi9csxkg1fff07ycilvwfaqw64fhq1cqn"))

(define rust-unicode-vo-0.1.0
  (crate-source "unicode-vo" "0.1.0"
                "151sha088v9jyfvbg5164xh4dk72g53b82xm4zzbf5dlagzqdlxi"))

(define rust-unicode-width-0.1.12
  (crate-source "unicode-width" "0.1.12"
                "1mk6mybsmi5py8hf8zy9vbgs4rw4gkdqdq3gzywd9kwf2prybxb8"))

(define rust-unicode-width-0.1.14
  (crate-source "unicode-width" "0.1.14"
                "1bzn2zv0gp8xxbxbhifw778a7fc93pa6a1kj24jgg9msj07f7mkx"))

(define rust-unicode-width-0.2.0
  (crate-source "unicode-width" "0.2.0"
                "1zd0r5vs52ifxn25rs06gxrgz8cmh4xpra922k0xlmrchib1kj0z"))

(define rust-unicode-width-0.2.1
  (crate-source "unicode-width" "0.2.1"
                "0k0mlq7xy1y1kq6cgv1r2rs2knn6rln3g3af50rhi0dkgp60f6ja"))

(define rust-unicode-width-0.2.2
  (crate-source "unicode-width" "0.2.2"
                "0m7jjzlcccw716dy9423xxh0clys8pfpllc5smvfxrzdf66h9b5l"))

(define rust-unicode-width-16-0.1.0
  (crate-source "unicode-width-16" "0.1.0"
                "1hmmsypbqqxni5fw1cbr0w5xa7xnvljac4ndss7bzxd0d81ibfly"))

(define rust-unicode-xid-0.0.4
  (crate-source "unicode-xid" "0.0.4"
                "1p5l9h3n3i53cp95fb65p8q3vbwib79ryd9z5z5h5kr9gl6qc7wc"))

(define rust-unicode-xid-0.2.4
  (crate-source "unicode-xid" "0.2.4"
                "131dfzf7d8fsr1ivch34x42c2d1ik5ig3g78brxncnn0r1sdyqpr"))

(define rust-unicode-xid-0.2.6
  (crate-source "unicode-xid" "0.2.6"
                "0lzqaky89fq0bcrh6jj6bhlz37scfd8c7dsj5dq7y32if56c1hgb"))

(define rust-uninitialized-0.0.2
  (crate-source "uninitialized" "0.0.2"
                "10by0nyjl44a4y7y2lgv1h03yycbbwghnvs0932pd0n3252smhbl"))

(define rust-universal-config-0.5.0
  (crate-source "universal-config" "0.5.0"
                "1ji69c2w67waxx7j7f9g2pmwxhj1qlvg1jacspjjjfpd4qhla0bb"))

(define rust-unsafe-libyaml-0.2.11
  (crate-source "unsafe-libyaml" "0.2.11"
                "0qdq69ffl3v5pzx9kzxbghzn0fzn266i1xn70y88maybz9csqfk7"))

(define rust-unsigned-varint-0.8.0
  (crate-source "unsigned-varint" "0.8.0"
                "01lwzxp0hf5p966fjq0lyz2x4l44b52py1fbffp9clabn9cnj1pb"))

(define rust-untrusted-0.7.1
  (crate-source "untrusted" "0.7.1"
                "0jkbqaj9d3v5a91pp3wp9mffvng1nhycx6sh4qkdd9qyr62ccmm1"))

(define rust-untrusted-0.9.0
  (crate-source "untrusted" "0.9.0"
                "1ha7ib98vkc538x0z60gfn0fc5whqdd85mb87dvisdcaifi6vjwf"))

(define rust-unty-0.0.4
  (crate-source "unty" "0.0.4"
                "1blhyv01qiv5sb72sal3xa1l8nzck3answawxkkiw3fd2x1phjbd"))

(define rust-upower-dbus-0.3.2
  (crate-source "upower_dbus" "0.3.2"
                "133g244g12acdzqhgkawkv2czvn4p12v18jzgwcfbclax5yxgcdh"))

(define rust-ureq-3.0.0-rc5
  (crate-source "ureq" "3.0.0-rc5"
                "0wzi4mih0ksjhxj0m39zzyy5qlr2kvha05dij1yghfzy0cfvvybp"))

(define rust-ureq-proto-0.2.3
  (crate-source "ureq-proto" "0.2.3"
                "1cwyr58lzlhhnj32r349s0f9rwni3giir49ak546yhhm5kyjjxx2"))

(define rust-url-2.5.2
  (crate-source "url" "2.5.2"
                "0v2dx50mx7xzl9454cl5qmpjnhkbahmn59gd3apyipbgyyylsy12"))

(define rust-url-2.5.4
  (crate-source "url" "2.5.4"
                "0q6sgznyy2n4l5lm16zahkisvc9nip9aa5q1pps7656xra3bdy1j"))

(define rust-url-2.5.7
  (crate-source "url" "2.5.7"
                "0nzghdv0kcksyvri0npxbjzyx2ihprks5k590y77bld355m17g08"))

(define rust-url-2.5.8
  (crate-source "url" "2.5.8"
                "1v8f7nx3hpr1qh76if0a04sj08k86amsq4h8cvpw6wvk76jahrzz"))

(define rust-urlencoding-2.1.3
  (crate-source "urlencoding" "2.1.3"
                "1nj99jp37k47n0hvaz5fvz7z6jd0sb4ppvfy3nphr1zbnyixpy6s"))

(define rust-usvg-0.42.0
  (crate-source "usvg" "0.42.0"
                "0cihjqi0nyl4jg812blhkm9n0dqw4fj3h908ggq1biw5mr1aakmq"))

(define rust-usvg-0.45.1
  (crate-source "usvg" "0.45.1"
                "1vs7gfhyxjgkgibgwfjniwrszfw0iivj1aq06hq8nfxfzc39pgl0"))

(define rust-utf-8-0.7.6
  (crate-source "utf-8" "0.7.6"
                "1a9ns3fvgird0snjkd3wbdhwd3zdpc2h5gpyybrfr6ra5pkqxk09"))

(define rust-utf16-iter-1.0.5
  (crate-source "utf16_iter" "1.0.5"
                "0ik2krdr73hfgsdzw0218fn35fa09dg2hvbi1xp3bmdfrp9js8y8"))

(define rust-utf7-imap-0.3.2
  (crate-source "utf7-imap" "0.3.2"
                "0pczz3pazxj6ypnz1n5v4w24llqh501vdpq910gpdhhz4rjn6cjy"))

(define rust-utf8-iter-1.0.4
  (crate-source "utf8_iter" "1.0.4"
                "1gmna9flnj8dbyd8ba17zigrp9c4c3zclngf5lnb5yvz1ri41hdn"))

(define rust-utf8-width-0.1.8
  (crate-source "utf8-width" "0.1.8"
                "14d08vrz878wqpmqw46yl5l1vwmdf00zx4i49z8iahdmf3cw14hj"))

(define rust-utf8parse-0.2.1
  (crate-source "utf8parse" "0.2.1"
                "02ip1a0az0qmc2786vxk2nqwsgcwf17d3a38fkf0q7hrmwh9c6vi"))

(define rust-utf8parse-0.2.2
  (crate-source "utf8parse" "0.2.2"
                "088807qwjq46azicqwbhlmzwrbkz7l4hpw43sdkdyyk524vdxaq6"))

(define rust-uuid-0.8.2
  (crate-source "uuid" "0.8.2"
                "1dy4ldcp7rnzjy56dxh7d2sgrcvn4q77y0a8r0a48946h66zjp5w"))

(define rust-uuid-1.11.0
  (crate-source "uuid" "1.11.0"
                "0sj4l28lif2wm4xrafdfgqjywjzv43wzp8nii9a4i539myhg1igq"))

(define rust-uuid-1.11.1
  (crate-source "uuid" "1.11.1"
                "1i2nlxkzfxsi0bz33z28ny4szk0r8fv65k33klk2w544zsss64xr"))

(define rust-uuid-1.15.1
  (crate-source "uuid" "1.15.1"
                "11ymndpddvsjlais2fkaq4ln3n5xzn1vlr4b2bkcr6034kil1xg0"))

(define rust-uuid-1.18.0
  (crate-source "uuid" "1.18.0"
                "1gn1vlggiwrdpizqcpc5hyxsqz9s5215bbay1b182mqn7rj9ccgk"))

(define rust-uuid-1.18.1
  (crate-source "uuid" "1.18.1"
                "18kh01qmfayn4psap52x8xdjkzw2q8bcbpnhhxjs05dr22mbi1rg"))

(define rust-uuid-1.19.0
  (crate-source "uuid" "1.19.0"
                "0jjbclx3f36fjl6jjh8f022q0m76v3cfh61y6z6jgl2b3f359q72"))

(define rust-uuid-1.7.0
  (crate-source "uuid" "1.7.0"
                "0aivp5ys7sg2izlj2sn6rr8p43vdcwg64naj8n0kqbd15iqcj37h"))

(define rust-uuid-1.8.0
  (crate-source "uuid" "1.8.0"
                "1h7wks153j08xmdk06wnza3is8pn6j37hihd3kfv95xsxrzwz0x1"))

(define rust-uuid-simd-0.8.0
  (crate-source "uuid-simd" "0.8.0"
                "1n0b40m988h52xj03dkcp4plrzvz56r7xha1d681jrjg5ci85c13"))

(define rust-uzers-0.12.1
  (crate-source "uzers" "0.12.1"
                "1pcpi9v90nr3q2y3i4pkac9c20r1nzaimvcm7vajmn770ksizy2d"))

(define rust-uzers-0.12.2
  (crate-source "uzers" "0.12.2"
                "13g1igzqpn5jvsbcp45rs2pw9fsxa1f8pp6j250mpqpy3bxpb0hb"))

(define rust-v-frame-0.3.9
  (crate-source "v_frame" "0.3.9"
                "1qkvb4ks33zck931vzqckjn36hkngj6l2cwmvfsnlpc7r0kpfsv6"))

(define rust-v4l-0.14.0
  (crate-source "v4l" "0.14.0"
                "00rl3h2a2a6k93c52y2ppqpp5pq6bp2z68skqmi9sya69ajgxyyq"))

(define rust-v4l-sys-0.3.0
  ;; TODO: Check bundled sources.
  (crate-source "v4l-sys" "0.3.0"
                "0c7sjm9wzqlaf5dsf5y98ck618j6ib0wjibr25nygw3njjdlw88n"))

(define rust-valuable-0.1.0
  (crate-source "valuable" "0.1.0"
                "0v9gp3nkjbl30z0fd56d8mx7w1csk86wwjhfjhr400wh9mfpw2w3"))

(define rust-valuable-0.1.1
  (crate-source "valuable" "0.1.1"
                "0r9srp55v7g27s5bg7a2m095fzckrcdca5maih6dy9bay6fflwxs"))

(define rust-value-bag-1.10.0
  (crate-source "value-bag" "1.10.0"
                "1lnsixdpi1ldms1adxyafyx7lyrqxhhskgwrjckmml6majmc9x1y"))

(define rust-value-bag-serde1-1.10.0
  (crate-source "value-bag-serde1" "1.10.0"
                "02sv7k5q2gf9ga91fj3k2rj3hrlw9m2r8v1kdv5cfngx6syp7dsb"))

(define rust-value-bag-sval2-1.10.0
  (crate-source "value-bag-sval2" "1.10.0"
                "0fm8lkzbf18x8hch9dqlrmxncxcp8iynb5scd643mi6a0akidaak"))

(define rust-vcpkg-0.2.15
  (crate-source "vcpkg" "0.2.15"
                "09i4nf5y8lig6xgj3f7fyrvzd3nlaw4znrihw8psidvv5yk4xkdc"))

(define rust-vec-extract-if-polyfill-0.1.0
  (crate-source "vec_extract_if_polyfill" "0.1.0"
                "173mk5vjn2vzn932ykxyfnd4qvjbvpkzrcvf1cqr49kwnrgwpja0"))

(define rust-vec-map-0.8.2
  (crate-source "vec_map" "0.8.2"
                "1481w9g1dw9rxp3l6snkdqihzyrd2f8vispzqmwjwsdyhw8xzggi"))

(define rust-version-check-0.9.4
  (crate-source "version_check" "0.9.4"
                "0gs8grwdlgh0xq660d7wr80x14vxbizmd8dbp29p2pdncx8lp1s9"))

(define rust-version-check-0.9.5
  (crate-source "version_check" "0.9.5"
                "0nhhi4i5x89gm911azqbn7avs9mdacw2i3vcz3cnmz3mv4rqz4hb"))

(define rust-version-compare-0.1.1
  (crate-source "version-compare" "0.1.1"
                "0acg4pmjdbmclg0m7yhijn979mdy66z3k8qrcnvn634f1gy456jp"))

(define rust-version-compare-0.2.0
  (crate-source "version-compare" "0.2.0"
                "12y9262fhjm1wp0aj3mwhads7kv0jz8h168nn5fb8b43nwf9abl5"))

(define rust-version-compare-0.2.1
  (crate-source "version-compare" "0.2.1"
                "03nziqxwnxlizl42cwsx33vi5xd2cf2jnszhh9rzay7g6xl8bhh3"))

(define rust-virtue-0.0.18
  (crate-source "virtue" "0.0.18"
                "1cgp79pzzs117kjlc3jnnkixbyaqri12j40mx2an41qhrymv27h5"))

(define rust-void-1.0.2
  (crate-source "void" "1.0.2"
                "0zc8f0ksxvmhvgx4fdg0zyn6vdnbxd2xv9hfx4nhzg6kbs4f80ka"))

(define rust-vparser-1.0.1
  (crate-source "vparser" "1.0.1"
                "0yjszxiqz9bwxd5qx4w8k1gcbgf1mi9wrk75d89443najyl3klzr"))

(define rust-vparser-1.1.0
  (crate-source "vparser" "1.1.0"
                "1l8qw1v5g1b2d3wsnrsd7314gq28ylyz279v2rsyjjvwcsydh5gr"))

(define rust-vsimd-0.8.0
  (crate-source "vsimd" "0.8.0"
                "0r4wn54jxb12r0x023r5yxcrqk785akmbddqkcafz9fm03584c2w"))

(define rust-vsock-0.5.2
  (crate-source "vsock" "0.5.2"
                "0qzd42wk8wrapl5ghbzf8j02zdlb6y2qkyg0vhsrdlbcqx56xnp2"))

(define rust-vstorage-0.4.0
  (crate-source "vstorage" "0.4.0"
                "0x2cgp40b4ifnvlzz0pkjvi5dfll091ray5158xymdy90mwh47v9"))

(define rust-vte-0.11.1
  (crate-source "vte" "0.11.1"
                "15r1ff4j8ndqj9vsyil3wqwxhhl7jsz5g58f31n0h1wlpxgjn0pm"))

(define rust-vte-0.14.1
  (crate-source "vte" "0.14.1"
                "0xy01fgkzb2080prh2ncd8949hm2248fc5wf1lryhdrhxzbxq7r3"))

(define rust-vte-0.15.0
  (crate-source "vte" "0.15.0"
                "1g9xgnw7q7zdwgfqa6zfcfsp92wn0j0h13kzsqy0dq3c80c414m5"))

(define rust-vte-generate-state-changes-0.1.1
  (crate-source "vte_generate_state_changes" "0.1.1"
                "1zs5q766q7jmc80c5c80gpzy4qpg5lnydf94mgdzrpy7h5q82myj"))

(define rust-vtparse-0.6.2
  (crate-source "vtparse" "0.6.2"
                "1l5yz9650zhkaffxn28cvfys7plcw2wd6drajyf41pshn37jm6vd"))

(define rust-wait-timeout-0.2.0
  (crate-source "wait-timeout" "0.2.0"
                "1xpkk0j5l9pfmjfh1pi0i89invlavfrd9av5xp0zhxgb29dhy84z"))

(define rust-wait-timeout-0.2.1
  (crate-source "wait-timeout" "0.2.1"
                "04azqv9mnfxgvnc8j2wp362xraybakh2dy1nj22gj51rdl93pb09"))

(define rust-waker-fn-1.1.0
  (crate-source "waker-fn" "1.1.0"
                "1jpfiis0frk2b36krqvk8264kgxk2dyhfzjsr8g3wah1nii2qnwx"))

(define rust-walkdir-2.5.0
  (crate-source "walkdir" "2.5.0"
                "0jsy7a710qv8gld5957ybrnc07gavppp963gs32xk4ag8130jy99"))

(define rust-want-0.3.0
  (crate-source "want" "0.3.0"
                "181b2zmwfq389x9n2g1n37cvcvvdand832zz6v8i1l8wrdlaks0w"))

(define rust-want-0.3.1
  (crate-source "want" "0.3.1"
                "03hbfrnvqqdchb5kgxyavb9jabwza0dmh2vw5kg0dq8rxl57d9xz"))

(define rust-wasi-0.11.0+wasi-snapshot-preview1
  (crate-source "wasi" "0.11.0+wasi-snapshot-preview1"
                "08z4hxwkpdpalxjps1ai9y7ihin26y9f476i53dv98v45gkqg3cw"))

(define rust-wasi-0.11.1+wasi-snapshot-preview1
  (crate-source "wasi" "0.11.1+wasi-snapshot-preview1"
                "0jx49r7nbkbhyfrfyhz0bm4817yrnxgd3jiwwwfv0zl439jyrwyc"))

(define rust-wasi-0.13.3+wasi-0.2.2
  (crate-source "wasi" "0.13.3+wasi-0.2.2"
                "1lnapbvdcvi3kc749wzqvwrpd483win2kicn1faa4dja38p6v096"))

(define rust-wasi-0.14.2+wasi-0.2.4
  (crate-source "wasi" "0.14.2+wasi-0.2.4"
                "1cwcqjr3dgdq8j325awgk8a715h0hg0f7jqzsb077n4qm6jzk0wn"))

(define rust-wasi-0.14.7+wasi-0.2.4
  (crate-source "wasi" "0.14.7+wasi-0.2.4"
                "133fq3mq7h65mzrsphcm7bbbx1gsz7srrbwh01624zin43g7hd48"))

(define rust-wasip2-1.0.1+wasi-0.2.4
  (crate-source "wasip2" "1.0.1+wasi-0.2.4"
                "1rsqmpspwy0zja82xx7kbkbg9fv34a4a2if3sbd76dy64a244qh5"))

(define rust-wasip2-1.0.2+wasi-0.2.9
  (crate-source "wasip2" "1.0.2+wasi-0.2.9"
                "1xdw7v08jpfjdg94sp4lbdgzwa587m5ifpz6fpdnkh02kwizj5wm"))

(define rust-wasite-0.1.0
  (crate-source "wasite" "0.1.0"
                "0nw5h9nmcl4fyf4j5d4mfdjfgvwi1cakpi349wc4zrr59wxxinmq"))

(define rust-wasm-bindgen-0.2.100
  (crate-source "wasm-bindgen" "0.2.100"
                "1x8ymcm6yi3i1rwj78myl1agqv2m86i648myy3lc97s9swlqkp0y"))

(define rust-wasm-bindgen-0.2.104
  (crate-source "wasm-bindgen" "0.2.104"
                "0b8f4l6pqm0bz0lj5xgwmchb6977n71vmh7srd0axwg93b011nn1"))

(define rust-wasm-bindgen-0.2.105
  (crate-source "wasm-bindgen" "0.2.105"
                "0q7aqnjwl9dn5sql46nbhzz63v3q0mdp3rsv7zcvn7s1zhypk5fs"))

(define rust-wasm-bindgen-0.2.106
  (crate-source "wasm-bindgen" "0.2.106"
                "1zc0pcyv0w1dhp8r7ybmmfjsf4g18q784h0k7mv2sjm67x1ryx8d"))

(define rust-wasm-bindgen-0.2.108
  (crate-source "wasm-bindgen" "0.2.108"
                "0rl5pn80sdhj2p2r28lp3k50a8mpppzgwzssz2f3jdqyxhq4l0k4"))

(define rust-wasm-bindgen-0.2.84
  (crate-source "wasm-bindgen" "0.2.84"
                "0fx5gh0b4n6znfa3blz92wn1k4bbiysyq9m95s7rn3gk46ydry1i"))

(define rust-wasm-bindgen-0.2.92
  (crate-source "wasm-bindgen" "0.2.92"
                "1a4mcw13nsk3fr8fxjzf9kk1wj88xkfsmnm0pjraw01ryqfm7qjb"))

(define rust-wasm-bindgen-0.2.93
  (crate-source "wasm-bindgen" "0.2.93"
                "1dfr7pka5kwvky2fx82m9d060p842hc5fyyw8igryikcdb0xybm8"))

(define rust-wasm-bindgen-0.2.95
  (crate-source "wasm-bindgen" "0.2.95"
                "0bpbvmxhil380gpv53smaypl8wc7sy7rq8apxfw349pn78v1x38j"))

(define rust-wasm-bindgen-0.2.99
  (crate-source "wasm-bindgen" "0.2.99"
                "15k3rzb3kjrxyqnh0916gq99mrpwhwy62smawxxc2w0x3llgcx54"))

(define rust-wasm-bindgen-backend-0.2.100
  (crate-source "wasm-bindgen-backend" "0.2.100"
                "1ihbf1hq3y81c4md9lyh6lcwbx6a5j0fw4fygd423g62lm8hc2ig"))

(define rust-wasm-bindgen-backend-0.2.104
  (crate-source "wasm-bindgen-backend" "0.2.104"
                "069vnhhn2j4w2gwd8rch6g8d3iwkrgi45fas6i3qm7glcrd9l737"))

(define rust-wasm-bindgen-backend-0.2.84
  (crate-source "wasm-bindgen-backend" "0.2.84"
                "1ffc0wb293ha56i66f830x7f8aa2xql69a21lrasy1ncbgyr1klm"))

(define rust-wasm-bindgen-backend-0.2.92
  (crate-source "wasm-bindgen-backend" "0.2.92"
                "1nj7wxbi49f0rw9d44rjzms26xlw6r76b2mrggx8jfbdjrxphkb1"))

(define rust-wasm-bindgen-backend-0.2.93
  (crate-source "wasm-bindgen-backend" "0.2.93"
                "0yypblaf94rdgqs5xw97499xfwgs1096yx026d6h88v563d9dqwx"))

(define rust-wasm-bindgen-backend-0.2.95
  (crate-source "wasm-bindgen-backend" "0.2.95"
                "0n53wgy78bgzgjwk0z69zbspzhv8p2a4zh69s4fzvpqdrb9x8vfb"))

(define rust-wasm-bindgen-backend-0.2.99
  (crate-source "wasm-bindgen-backend" "0.2.99"
                "0ycwa4c68j34687k513djgyy2asn3fw3yp4g9rkq2kvbchwbp2az"))

(define rust-wasm-bindgen-futures-0.4.34
  (crate-source "wasm-bindgen-futures" "0.4.34"
                "0m0lnnnhs9ni4dn9vz74prsjz8bdcf8dvnznd5ljch5s279f06gj"))

(define rust-wasm-bindgen-futures-0.4.42
  (crate-source "wasm-bindgen-futures" "0.4.42"
                "1h322zjvpjllcpj7dahfxjsv6inkr6y0baw7nkdwivr1c4v19g3n"))

(define rust-wasm-bindgen-futures-0.4.49
  (crate-source "wasm-bindgen-futures" "0.4.49"
                "18pc937q8kg549czxk3ah57h9vb7q56c62zz9qcfk17a8jdns5rq"))

(define rust-wasm-bindgen-futures-0.4.50
  (crate-source "wasm-bindgen-futures" "0.4.50"
                "0q8ymi6i9r3vxly551dhxcyai7nc491mspj0j1wbafxwq074fpam"))

(define rust-wasm-bindgen-futures-0.4.55
  (crate-source "wasm-bindgen-futures" "0.4.55"
                "186l2nhznpbxcl2475dlm1pd7dfk5lqnz8frgk67qpkddh88h7sm"))

(define rust-wasm-bindgen-futures-0.4.56
  (crate-source "wasm-bindgen-futures" "0.4.56"
                "0z6f0zkylpgbfb7dkh7a85dxdwm57q7c2np2bngfxzh4sqi9cvc3"))

(define rust-wasm-bindgen-futures-0.4.58
  (crate-source "wasm-bindgen-futures" "0.4.58"
                "0vqywn9df5i6mms3sw47v3kj7rzx8ryghqq0xb4jk05fs1zyg9kh"))

(define rust-wasm-bindgen-macro-0.2.100
  (crate-source "wasm-bindgen-macro" "0.2.100"
                "01xls2dvzh38yj17jgrbiib1d3nyad7k2yw9s0mpklwys333zrkz"))

(define rust-wasm-bindgen-macro-0.2.104
  (crate-source "wasm-bindgen-macro" "0.2.104"
                "06d1m5bg272h6jabq0snm7c50fifjz6r20f5hqlmz7y5wivh99kw"))

(define rust-wasm-bindgen-macro-0.2.105
  (crate-source "wasm-bindgen-macro" "0.2.105"
                "1hjrmfnnjcksmf4kizsb78a0apr6ym73iwlhhszsf14ya0s469h4"))

(define rust-wasm-bindgen-macro-0.2.106
  (crate-source "wasm-bindgen-macro" "0.2.106"
                "1czfwzhqrkzyyhd3g58mdwb2jjk4q2pl9m1fajyfvfpq70k0vjs8"))

(define rust-wasm-bindgen-macro-0.2.108
  (crate-source "wasm-bindgen-macro" "0.2.108"
                "026nnvakp0w6j3ghpcxn31shj9wx8bv8x7nk3gkk40klkjfj72q0"))

(define rust-wasm-bindgen-macro-0.2.84
  (crate-source "wasm-bindgen-macro" "0.2.84"
                "1idlq28awqhq8rclb22rn5xix82w9a4rgy11vkapzhzd1dygf8ac"))

(define rust-wasm-bindgen-macro-0.2.92
  (crate-source "wasm-bindgen-macro" "0.2.92"
                "09npa1srjjabd6nfph5yc03jb26sycjlxhy0c2a1pdrpx4yq5y51"))

(define rust-wasm-bindgen-macro-0.2.93
  (crate-source "wasm-bindgen-macro" "0.2.93"
                "1kycd1xfx4d9xzqknvzbiqhwb5fzvjqrrn88x692q1vblj8lqp2q"))

(define rust-wasm-bindgen-macro-0.2.95
  (crate-source "wasm-bindgen-macro" "0.2.95"
                "0mic8b2vab1a91m6x3hjxkwz23094bq1cwhnszarsnlggyz894z7"))

(define rust-wasm-bindgen-macro-0.2.99
  (crate-source "wasm-bindgen-macro" "0.2.99"
                "1znlcrk5bvisr3vscwlqkdby959n3sb367zgdzpjwjd7v4giiiic"))

(define rust-wasm-bindgen-macro-support-0.2.100
  (crate-source "wasm-bindgen-macro-support" "0.2.100"
                "1plm8dh20jg2id0320pbmrlsv6cazfv6b6907z19ys4z1jj7xs4a"))

(define rust-wasm-bindgen-macro-support-0.2.104
  (crate-source "wasm-bindgen-macro-support" "0.2.104"
                "1mr18kx7ima1pmsqlkk982q4a0vf3r8s1x6901jb59sd1prd41wz"))

(define rust-wasm-bindgen-macro-support-0.2.105
  (crate-source "wasm-bindgen-macro-support" "0.2.105"
                "1g3ydkp5js5qg56a30w9v090v5byslay2lidjiifa8pkv4ww62s2"))

(define rust-wasm-bindgen-macro-support-0.2.106
  (crate-source "wasm-bindgen-macro-support" "0.2.106"
                "0h6ddq6cc6jf9phsdh2a3x8lpjhmkya86ihfz3fdk4jzrpamkyyf"))

(define rust-wasm-bindgen-macro-support-0.2.108
  (crate-source "wasm-bindgen-macro-support" "0.2.108"
                "0m9sj475ypgifbkvksjsqs2gy3bq96f87ychch784m4gspiblmjj"))

(define rust-wasm-bindgen-macro-support-0.2.84
  (crate-source "wasm-bindgen-macro-support" "0.2.84"
                "1xm56lpi0rihh8ny7x085dgs3jdm47spgqflb98wghyadwq83zra"))

(define rust-wasm-bindgen-macro-support-0.2.92
  (crate-source "wasm-bindgen-macro-support" "0.2.92"
                "1dqv2xs8zcyw4kjgzj84bknp2h76phmsb3n7j6hn396h4ssifkz9"))

(define rust-wasm-bindgen-macro-support-0.2.93
  (crate-source "wasm-bindgen-macro-support" "0.2.93"
                "0dp8w6jmw44srym6l752nkr3hkplyw38a2fxz5f3j1ch9p3l1hxg"))

(define rust-wasm-bindgen-macro-support-0.2.95
  (crate-source "wasm-bindgen-macro-support" "0.2.95"
                "0s7g6glb85lyx2pj83shbmg4d50mvqhb2c2qk2j28yigaxbspii6"))

(define rust-wasm-bindgen-macro-support-0.2.99
  (crate-source "wasm-bindgen-macro-support" "0.2.99"
                "1hihsgyg0kf46kjhgfv8x5g9x0q1d0aizj6n7s84ag1xfrdskmrh"))

(define rust-wasm-bindgen-shared-0.2.100
  (crate-source "wasm-bindgen-shared" "0.2.100"
                "0gffxvqgbh9r9xl36gprkfnh3w9gl8wgia6xrin7v11sjcxxf18s"))

(define rust-wasm-bindgen-shared-0.2.104
  (crate-source "wasm-bindgen-shared" "0.2.104"
                "1la1xj9v3gmawnlyi7lc3mb3xi447r6frb98hi2fb9m1nb47vmms"))

(define rust-wasm-bindgen-shared-0.2.105
  (crate-source "wasm-bindgen-shared" "0.2.105"
                "0xmgnvf5m91lw6avjxha1v76bm27g2q5jw7c4lyb7g44ijiiiwkn"))

(define rust-wasm-bindgen-shared-0.2.106
  (crate-source "wasm-bindgen-shared" "0.2.106"
                "1d0dh3jn77qz67n5zh0s3rvzlbjv926p0blq5bvng2v4gq2kiifb"))

(define rust-wasm-bindgen-shared-0.2.108
  (crate-source "wasm-bindgen-shared" "0.2.108"
                "04ix7v99rvj5730553j58pqsrwpf9sqazr60y3cchx5cr60ba08z"))

(define rust-wasm-bindgen-shared-0.2.84
  (crate-source "wasm-bindgen-shared" "0.2.84"
                "0pcvk1c97r1pprzfaxxn359r0wqg5bm33ylbwgjh8f4cwbvzwih0"))

(define rust-wasm-bindgen-shared-0.2.92
  (crate-source "wasm-bindgen-shared" "0.2.92"
                "15kyavsrna2cvy30kg03va257fraf9x00ny554vxngvpyaa0q6dg"))

(define rust-wasm-bindgen-shared-0.2.93
  (crate-source "wasm-bindgen-shared" "0.2.93"
                "1104bny0hv40jfap3hp8jhs0q4ya244qcrvql39i38xlghq0lan6"))

(define rust-wasm-bindgen-shared-0.2.95
  (crate-source "wasm-bindgen-shared" "0.2.95"
                "1386q7mvv5ky003hcc6yyxpid3y1m7fy0l920i3z3ab60vqhkz35"))

(define rust-wasm-bindgen-shared-0.2.99
  (crate-source "wasm-bindgen-shared" "0.2.99"
                "19h61snrhh1qhb5gz6zyb89l7fbj1fhmxcvi09p9l0mav8zsnfll"))

(define rust-wasm-bindgen-test-0.3.50
  (crate-source "wasm-bindgen-test" "0.3.50"
                "1hsjc60wynlhgw02p32pgb93303pqmsdfxj67gxdkdm37kixbj36"))

(define rust-wasm-bindgen-test-0.3.56
  (crate-source "wasm-bindgen-test" "0.3.56"
                "0v1cqb98k1k5vxycc5ara469q2xrka058ap7q3ps3lv5s9k0xs95"))

(define rust-wasm-bindgen-test-macro-0.3.50
  (crate-source "wasm-bindgen-test-macro" "0.3.50"
                "16znd6wz79v2i3b2sf5n4ld2kcci8br3wcx7z5c9c07sqln09m8p"))

(define rust-wasm-bindgen-test-macro-0.3.56
  (crate-source "wasm-bindgen-test-macro" "0.3.56"
                "1hgzkaldrgi3vk64rxi2jixmxd275zklif6jxa5h5rnw2rbk6l3i"))

(define rust-wasm-encoder-0.227.1
  (crate-source "wasm-encoder" "0.227.1"
                "08kqd9lmpncf10p9y60lq7wani217l7ppcj36hc0ggvz5vq75fw0"))

(define rust-wasm-metadata-0.227.1
  (crate-source "wasm-metadata" "0.227.1"
                "07d94b4p68xl8y5nqcssg11gkpfcazlbwmksx5s6dfmvmgxg07nf"))

(define rust-wasm-timer-0.2.5
  (crate-source "wasm-timer" "0.2.5"
                "0zsyijv3wgj9p4q47a4awla8j4kw33jd7da2fsd1wml0nh6wn3my"))

(define rust-wasmparser-0.227.1
  (crate-source "wasmparser" "0.227.1"
                "1ckqsv60i08fyhds7vaxgcwqhwzv5p3ckk4vmdhr8g7vfkbwll8g"))

(define rust-watchman-client-0.9.0
  (crate-source "watchman_client" "0.9.0"
                "0lpvdkvf500cr6454c722m9q15y3zxxq19sg1phsm9s3njdlrg48"))

(define rust-wayland-backend-0.3.11
  (crate-source "wayland-backend" "0.3.11"
                "0dcvwkhz45gsm7f9dwr31pxijkhpza09a4vb3blsv9a8631k6fk7"))

(define rust-wayland-backend-0.3.2
  (crate-source "wayland-backend" "0.3.2"
                "1r7vz56z6ixfbljraxl4q59g43jfb6i9qkaksi704pzlfgfjs58r"))

(define rust-wayland-backend-0.3.7
  (crate-source "wayland-backend" "0.3.7"
                "1xhnh0mn4cv0wmq3zcm0iic2sbhsz4qdra6kb58x8l51sz73ar85"))

(define rust-wayland-backend-0.3.8
  (crate-source "wayland-backend" "0.3.8"
                "1gs7dw6s3lp9g6g0rhk4bh66wl41jnbkd27c6ynhv1x3xac8j85p"))

(define rust-wayland-client-0.31.1
  (crate-source "wayland-client" "0.31.1"
                "1lryhibzmi4hb3jpbraj623l110f6rgp0migpxrm8vrl8wixb9qw"))

(define rust-wayland-client-0.31.11
  (crate-source "wayland-client" "0.31.11"
                "17a4vl5qw4jnnh2azm0d3kcpajyb9qz4psv448zpj86w83l4fsn6"))

(define rust-wayland-client-0.31.7
  (crate-source "wayland-client" "0.31.7"
                "105j23dj1k36rpvv3nk5v3lm99gs029k3k429kbnzxv9zk9ljqmn"))

(define rust-wayland-client-0.31.8
  (crate-source "wayland-client" "0.31.8"
                "0gzpr9gdd8yk1crflxngg5iwa1szyyzp4i4zbgpslf1nsgihs4n2"))

(define rust-wayland-csd-frame-0.3.0
  (crate-source "wayland-csd-frame" "0.3.0"
                "0zjcmcqprfzx57hlm741n89ssp4sha5yh5cnmbk2agflvclm0p32"))

(define rust-wayland-cursor-0.31.0
  (crate-source "wayland-cursor" "0.31.0"
                "0nxdyyg3a3649n316fbnm8rak5k90j580kfnfxn9src6x45a4jm4"))

(define rust-wayland-cursor-0.31.11
  (crate-source "wayland-cursor" "0.31.11"
                "0agb7ryc8p7bnylh00jvp44sl3yn4rbzg2crksqp24l8192cqz24"))

(define rust-wayland-cursor-0.31.7
  (crate-source "wayland-cursor" "0.31.7"
                "0k4yijr0rxlqw15clzbh6a3jd760l1xz3zg0gxg07c7xmb1qpc1j"))

(define rust-wayland-cursor-0.31.8
  (crate-source "wayland-cursor" "0.31.8"
                "0pccjqiln1izjbdzprqiijhad4k00wmr5r003a44h1v5nv5jjc59"))

(define rust-wayland-protocols-0.31.2
  (crate-source "wayland-protocols" "0.31.2"
                "1x310l1p6p3p3l76nl1l2yava9408dy77s605917zadlp1jz70cg"))

(define rust-wayland-protocols-0.32.5
  (crate-source "wayland-protocols" "0.32.5"
                "13pmq788d2w9c7sqpjjxi1344bzq1g1ja4vlaa4rlvjfgkjsvl3w"))

(define rust-wayland-protocols-0.32.6
  (crate-source "wayland-protocols" "0.32.6"
                "1z0yahh48x8qzdbcallmxn5am5897hkk5d7p51ly6dwvhr3cz087"))

(define rust-wayland-protocols-0.32.9
  (crate-source "wayland-protocols" "0.32.9"
                "00cripl4m7hzhl0gzp4bqayal8n0zlf1llnj7cl73zgvfpnr19zg"))

(define rust-wayland-protocols-experimental-20250721.0.1
  (crate-source "wayland-protocols-experimental" "20250721.0.1"
                "1cfbimd2qbbcgv21i3l7kq3pm6lvrjbb7d6pj33sxjld29izi8a0"))

(define rust-wayland-protocols-misc-0.3.9
  (crate-source "wayland-protocols-misc" "0.3.9"
                "176i0xirlaaif6arj6l4ay8r2qxr9fsahrzh7z8gz2zba7ak7zid"))

(define rust-wayland-protocols-plasma-0.3.5
  (crate-source "wayland-protocols-plasma" "0.3.5"
                "1g8jxv1k3zdbcqmhr4fpghgibp2940hz45sm2pmwfs7f92swlccv"))

(define rust-wayland-protocols-plasma-0.3.9
  (crate-source "wayland-protocols-plasma" "0.3.9"
                "0cn0qbr99snsh41p34qv11bvylbqk2xzii47k4kv6yh7ghji8ym0"))

(define rust-wayland-protocols-wlr-0.2.0
  (crate-source "wayland-protocols-wlr" "0.2.0"
                "1mjww9psk2nc5hm2q4s3qas30rbzfg1sb6qgw518fbbcdfvn27xd"))

(define rust-wayland-protocols-wlr-0.3.5
  (crate-source "wayland-protocols-wlr" "0.3.5"
                "08j0xfpfqv6kzbfzmdv6crfzalxbxc2n5m9hc4qkqg4jrpv14bkq"))

(define rust-wayland-protocols-wlr-0.3.6
  (crate-source "wayland-protocols-wlr" "0.3.6"
                "1cpqb0d4ryf87x2wgca5n71wilhvc0jjva0zasbdgalmypk052i4"))

(define rust-wayland-protocols-wlr-0.3.9
  (crate-source "wayland-protocols-wlr" "0.3.9"
                "1v3qbg18vsb3i62c6042xhjm7dcflmylzjlhl0w9kks3xmilkngg"))

(define rust-wayland-scanner-0.31.0
  (crate-source "wayland-scanner" "0.31.0"
                "1760n887j18lzd1ni087q7jzsmpcf7ny3dq2698zkjb56r02i3pv"))

(define rust-wayland-scanner-0.31.5
  (crate-source "wayland-scanner" "0.31.5"
                "1hv16shy6j32hi9i0r54pyk5pw3q7qfpkffmwchi3z75n80j0zsr"))

(define rust-wayland-scanner-0.31.6
  (crate-source "wayland-scanner" "0.31.6"
                "110ldnyfxjqvjssir1jf3ndlci7xy9lpv4aqg775y518bpyxlvw9"))

(define rust-wayland-scanner-0.31.7
  (crate-source "wayland-scanner" "0.31.7"
                "1qqalp551blcxjzx80zvs7ckc19k966892zxpm81kacxqjfixjsl"))

(define rust-wayland-sys-0.31.1
  ;; TODO: Check bundled sources.
  (crate-source "wayland-sys" "0.31.1"
                "1bxpwamgagpxa8p9m798gd3g6rwj2m4sbdvc49zx05jjzzmci80m"))

(define rust-wayland-sys-0.31.5
  ;; TODO: Check bundled sources.
  (crate-source "wayland-sys" "0.31.5"
                "02cyl94ydazgjdjf7asm2phni8h62j4cg4pwr6sy7lwfiq6sra7g"))

(define rust-wayland-sys-0.31.6
  ;; TODO: Check bundled sources.
  (crate-source "wayland-sys" "0.31.6"
                "05b6i4lg2qrrz7l4h2b5fd7blkkvxq34i1yvlngsmmbpkhwvpknv"))

(define rust-wayland-sys-0.31.7
  ;; TODO: Check bundled sources.
  (crate-source "wayland-sys" "0.31.7"
                "0hk157yawv9y7aj7fxbldhlvv8p33c65v3nv85mq4m91h919p51l"))

(define rust-web-sys-0.3.61
  ;; TODO: Check bundled sources.
  (crate-source "web-sys" "0.3.61"
                "15qbbdbhyz02srvm01p0cvlh0pvmbbin9hislp0yx8rvnbs9jfz3"))

(define rust-web-sys-0.3.69
  ;; TODO: Check bundled sources.
  (crate-source "web-sys" "0.3.69"
                "1vqkxk935xa8zcnsi4bd88sb267ly2i24xl1yiq26d1n32hskbvp"))

(define rust-web-sys-0.3.70
  ;; TODO: Check bundled sources.
  (crate-source "web-sys" "0.3.70"
                "1h1jspkqnrx1iybwhwhc3qq8c8fn4hy5jcf0wxjry4mxv6pymz96"))

(define rust-web-sys-0.3.72
  ;; TODO: Check bundled sources.
  (crate-source "web-sys" "0.3.72"
                "04k19hilj9r8sx6q20fz853149gfpmf83yk2zvq0s14c2288nj7n"))

(define rust-web-sys-0.3.76
  ;; TODO: Check bundled sources.
  (crate-source "web-sys" "0.3.76"
                "1g0j3kii20mh7hqmdajz2r4c21zynb9h9h026wymalkx88ip5p84"))

(define rust-web-sys-0.3.77
  ;; TODO: Check bundled sources.
  (crate-source "web-sys" "0.3.77"
                "1lnmc1ffbq34qw91nndklqqm75rasaffj2g4f8h1yvqqz4pdvdik"))

(define rust-web-sys-0.3.82
  ;; TODO: Check bundled sources.
  (crate-source "web-sys" "0.3.82"
                "18d9k5f92czaj27zmw20nzcnax5rdcx68r3s3ypg8irss309a7rs"))

(define rust-web-sys-0.3.83
  ;; TODO: Check bundled sources.
  (crate-source "web-sys" "0.3.83"
                "1b1pw450ig62xr0cy1wfjlbahvmi725jl64d150j0hacfy6q4clv"))

(define rust-web-sys-0.3.85
  ;; TODO: Check bundled sources.
  (crate-source "web-sys" "0.3.85"
                "1645c202gyw21m6kxw4ya81vrapl40hlb8m9iqhjj8fra7jk4bii"))

(define rust-web-time-1.1.0
  (crate-source "web-time" "1.1.0"
                "1fx05yqx83dhx628wb70fyy10yjfq1jpl20qfqhdkymi13rq0ras"))

(define rust-webbrowser-1.0.6
  (crate-source "webbrowser" "1.0.6"
                "15sc38k0j8ssgr4li6bgavmad8r46h4rc0zs68x3w8c5ywz29w80"))

(define rust-webpki-root-certs-0.26.7
  (crate-source "webpki-root-certs" "0.26.7"
                "0p15xwdlibwqlmkqjb6qqikypyxqb0lwxf70rxa01wzipm4xmmcw"))

(define rust-webpki-root-certs-1.0.5
  (crate-source "webpki-root-certs" "1.0.5"
                "1p05rj22vamgs9n34326vypxy3klmdbphqrjrxl3y4cb8309z8in"))

(define rust-webpki-roots-0.26.7
  (crate-source "webpki-roots" "0.26.7"
                "0zpykqqk4jnrx55jc8wcysnprhfdcwh35dsiwhm2fybydgqjyr2x"))

(define rust-webpki-roots-1.0.4
  (crate-source "webpki-roots" "1.0.4"
                "07jp2zgj3hjb60m1nwrasixdwazmzhh9y4bryy66wz6457q8x1xj"))

(define rust-weezl-0.1.10
  (crate-source "weezl" "0.1.10"
                "1wqnxqn8n90bgazs6djlibf58ppdxki4slblwp9lgnq0fwkv6ld7"))

(define rust-weezl-0.1.12
  (crate-source "weezl" "0.1.12"
                "122a1dhha6cib5az4ihcqlh60ns2bi6rskdv875p94lbvj6wk2m2"))

(define rust-weezl-0.1.8
  (crate-source "weezl" "0.1.8"
                "10lhndjgs6y5djpg3b420xngcr6jkmv70q8rb1qcicbily35pa2k"))

(define rust-wezterm-bidi-0.2.3
  (crate-source "wezterm-bidi" "0.2.3"
                "1v7kwmnxfplv9kgdmamn6csbn2ag5xjr0y6gs797slk0alsnw2hc"))

(define rust-wezterm-blob-leases-0.1.0
  (crate-source "wezterm-blob-leases" "0.1.0"
                "02z6m7p9vf4wvr9g2qv1kxp6zaxxli4ziphhcjbnivbyvw55wnlf"))

(define rust-wezterm-blob-leases-0.1.1
  (crate-source "wezterm-blob-leases" "0.1.1"
                "1dwf8bm3cwdi37fandwbk7nsfhn9spv4wm0l86gf551xv7vaybb9"))

(define rust-wezterm-color-types-0.3.0
  (crate-source "wezterm-color-types" "0.3.0"
                "15j29f60p1dc0msx50x940niyv9d5zpynavpcc6jf44hbkrixs3x"))

(define rust-wezterm-dynamic-0.2.1
  (crate-source "wezterm-dynamic" "0.2.1"
                "1b6mrk09xxiz66dj3912kmiq8rl7dqig6rwminkfmmhg287bcajz"))

(define rust-wezterm-dynamic-derive-0.1.1
  (crate-source "wezterm-dynamic-derive" "0.1.1"
                "0nspip7gwzmfn66fbnbpa2yik2sb97nckzmgir25nr4wacnwzh26"))

(define rust-wezterm-input-types-0.1.0
  (crate-source "wezterm-input-types" "0.1.0"
                "0zp557014d458a69yqn9dxfy270b6kyfdiynr5p4algrb7aas4kh"))

(define rust-wgpu-0.19.4
  (crate-source "wgpu" "0.19.4"
                "009cfqxabinkwmk93wm08gj8nz7d4hl1m17imfxgxg1aplfk3myb"))

(define rust-wgpu-25.0.2
  (crate-source "wgpu" "25.0.2"
                "1nbx35282pg22qynl7rk58xvcvpm75rn9hqbv3j2niqry6cb73zc"))

(define rust-wgpu-27.0.1
  (crate-source "wgpu" "27.0.1"
                "0xvsrmcp862zh3sd6nkr0cbsznna4c3l1hrilzkms4nygjn8prmz"))

(define rust-wgpu-core-0.19.4
  (crate-source "wgpu-core" "0.19.4"
                "0fmzd5rjga26yh96qi57244sv6y25d7pc94yk9f9xflrzhjlbf98"))

(define rust-wgpu-core-25.0.2
  (crate-source "wgpu-core" "25.0.2"
                "0075aj6shyc86bwdvsvacjv0s5jm0v4fwsiwc4fm2s43dwcq5f7p"))

(define rust-wgpu-core-27.0.3
  (crate-source "wgpu-core" "27.0.3"
                "1rya571frb73y32iwnhnxada31dk64vzj6b1nabihfsl2pjmv9r7"))

(define rust-wgpu-core-deps-apple-25.0.0
  (crate-source "wgpu-core-deps-apple" "25.0.0"
                "0g89bl9n99diw3j5sk3s8qsazy3br8wmq11vbhc7nswv4frqim6g"))

(define rust-wgpu-core-deps-apple-27.0.0
  (crate-source "wgpu-core-deps-apple" "27.0.0"
                "0cyjas0gx6wxjshv3f25p7gvqycnl7ckyphxaqlwgq4visaswwh7"))

(define rust-wgpu-core-deps-emscripten-25.0.0
  (crate-source "wgpu-core-deps-emscripten" "25.0.0"
                "0i84q3r5q5ih3hqlwgvgb1smfiyk97q7kimc74jya61qxfndg6ph"))

(define rust-wgpu-core-deps-emscripten-27.0.0
  (crate-source "wgpu-core-deps-emscripten" "27.0.0"
                "1dd69pd9z8m4a88h7qmkcjr20qmnfhkxn7fqrwz83c4m992c6smh"))

(define rust-wgpu-core-deps-windows-linux-android-25.0.0
  (crate-source "wgpu-core-deps-windows-linux-android" "25.0.0"
                "0ilfyld68pwq2ywgamnz6d45f9ff7bv49m49r2kvm64wgxgzp9fb"))

(define rust-wgpu-core-deps-windows-linux-android-27.0.0
  (crate-source "wgpu-core-deps-windows-linux-android" "27.0.0"
                "1hq3ka3mm507fysc3y017hp19bdj8a95mw1086778w8ssqkp06bi"))

(define rust-wgpu-hal-0.19.5
  (crate-source "wgpu-hal" "0.19.5"
                "00r7am82cmli890ww9rcjzbjyfscsnr2cqw1anl12rnqbz2wzaxz"))

(define rust-wgpu-hal-25.0.2
  (crate-source "wgpu-hal" "25.0.2"
                "05xdnkm5syz6jxaknwsxym8n83xzap6kqiyipd3kglykwizpcs7r"))

(define rust-wgpu-hal-27.0.4
  (crate-source "wgpu-hal" "27.0.4"
                "1khk8gb81izrd9815fdy4ywq1fsczynswwgz9ay9iqbyqmhwn8av"))

(define rust-wgpu-types-0.19.2
  (crate-source "wgpu-types" "0.19.2"
                "01cqjr9qvp34bgjlnrj27wwh7mp7xghlwjbny5pv8y1zn2gzywdn"))

(define rust-wgpu-types-25.0.0
  (crate-source "wgpu-types" "25.0.0"
                "1g780ryy2rr7fvd6l21fbgc4v40dahjl79gwlgdqxvm8q9h9991a"))

(define rust-wgpu-types-27.0.1
  (crate-source "wgpu-types" "27.0.1"
                "1sx5blmyas6k40ilhlrfsy31xcvcf0l1dn9dgxrxp42r756gip5g"))

(define rust-which-4.4.0
  (crate-source "which" "4.4.0"
                "0sd24r17q4j3hc2yjjrg9q4qya1y4n9zq0bj9c2rla1bqn2cfh94"))

(define rust-which-4.4.2
  (crate-source "which" "4.4.2"
                "1ixzmx3svsv5hbdvd8vdhd3qwvf6ns8jdpif1wmwsy10k90j9fl7"))

(define rust-which-8.0.0
  (crate-source "which" "8.0.0"
                "07dsqyvvyaqp3dbj4cdl3ib5fxhdf29l6vihm3pcihq666avpynk"))

(define rust-whoami-1.6.0
  (crate-source "whoami" "1.6.0"
                "19q2vm5ax3bgwffbywn4ad62anc1f4l1ky61h0y2qjdb30qx3539"))

(define rust-whoami-1.6.1
  (crate-source "whoami" "1.6.1"
                "0zg9sz669vhqyxysn4lymnianj29jxs2vl6k2lqcl0kp0yslsjjx"))

(define rust-widestring-1.1.0
  (crate-source "widestring" "1.1.0"
                "048kxd6iykzi5la9nikpc5hvpp77hmjf1sw43sl3z2dcdrmx66bj"))

(define rust-widestring-1.2.1
  (crate-source "widestring" "1.2.1"
                "0wg4qdbs70xqnlbm8wb0bs4idm2mxk3b6kaqwllsncmb2cqrq1kj"))

(define rust-winapi-0.3.9
  (crate-source "winapi" "0.3.9"
                "06gl025x418lchw1wxj64ycr7gha83m44cjr5sarhynd9xkrm0sw"))

(define rust-winapi-i686-pc-windows-gnu-0.4.0
  (crate-source "winapi-i686-pc-windows-gnu" "0.4.0"
                "1dmpa6mvcvzz16zg6d5vrfy4bxgg541wxrcip7cnshi06v38ffxc"))

(define rust-winapi-util-0.1.11
  (crate-source "winapi-util" "0.1.11"
                "08hdl7mkll7pz8whg869h58c1r9y7in0w0pk8fm24qc77k0b39y2"))

(define rust-winapi-util-0.1.5
  (crate-source "winapi-util" "0.1.5"
                "0y71bp7f6d536czj40dhqk0d55wfbbwqfp2ymqf1an5ibgl6rv3h"))

(define rust-winapi-util-0.1.9
  (crate-source "winapi-util" "0.1.9"
                "1fqhkcl9scd230cnfj8apfficpf5c9vhwnk4yy9xfc1sw69iq8ng"))

(define rust-winapi-wsapoll-0.1.2
  (crate-source "winapi-wsapoll" "0.1.2"
                "0a1zxmpvxaw75y4lwavi6qbq95cnrz83a5p84rarjxn5g7vcbbqy"))

(define rust-winapi-x86-64-pc-windows-gnu-0.4.0
  (crate-source "winapi-x86_64-pc-windows-gnu" "0.4.0"
                "0gqq64czqb64kskjryj8isp62m2sgvx25yyj3kpc2myh85w24bki"))

(define rust-window-clipboard-0.4.1
  (crate-source "window_clipboard" "0.4.1"
                "0vfb7cfm568xk6y3y2hbq9444ah0grsxhypfvbwk7i1qc3a95mpn"))

(define rust-windows-0.42.0
  (crate-source "windows" "0.42.0"
                "1mgm53gadj3smnia5gv28lcy2j6c881bn8fmcl3yflx7k8rvm1h2"))

(define rust-windows-0.52.0
  (crate-source "windows" "0.52.0"
                "1gnh210qjlprpd1szaq04rjm1zqgdm9j7l9absg0kawi2rwm72p4"))

(define rust-windows-0.54.0
  (crate-source "windows" "0.54.0"
                "0j8vd8sg2rbln6g3a608qg1a7r2lwxcga78mmxjjin5ybmrfallj"))

(define rust-windows-0.56.0
  (crate-source "windows" "0.56.0"
                "0cp10nzrqgrlk91dpwxjcpzyy6imr5vxr5f898pss7nz3gq9vrhx"))

(define rust-windows-0.58.0
  (crate-source "windows" "0.58.0"
                "1dkjj94b0gn91nn1n22cvm4afsj98f5qrhcl3112v6f4jcfx816x"))

(define rust-windows-0.60.0
  (crate-source "windows" "0.60.0"
                "0aa5jgm86j0yzfwkygbwwwjs80dzg9173imic8z7g6bs9kkp9y6x"))

(define rust-windows-0.61.3
  (crate-source "windows" "0.61.3"
                "14v8dln7i4ccskd8danzri22bkjkbmgzh284j3vaxhd4cykx7awv"))

(define rust-windows-0.62.2
  (crate-source "windows" "0.62.2"
                "10457l9ihrbw8j79z2v4plyjxkf6xvb5npd0lqwmkh702gpaszsj"))

(define rust-windows-aarch64-gnullvm-0.42.2
  (crate-source "windows_aarch64_gnullvm" "0.42.2"
                "1y4q0qmvl0lvp7syxvfykafvmwal5hrjb4fmv04bqs0bawc52yjr"))

(define rust-windows-aarch64-gnullvm-0.48.0
  (crate-source "windows_aarch64_gnullvm" "0.48.0"
                "1g71yxi61c410pwzq05ld7si4p9hyx6lf5fkw21sinvr3cp5gbli"))

(define rust-windows-aarch64-gnullvm-0.48.5
  (crate-source "windows_aarch64_gnullvm" "0.48.5"
                "1n05v7qblg1ci3i567inc7xrkmywczxrs1z3lj3rkkxw18py6f1b"))

(define rust-windows-aarch64-gnullvm-0.52.0
  (crate-source "windows_aarch64_gnullvm" "0.52.0"
                "1shmn1kbdc0bpphcxz0vlph96bxz0h1jlmh93s9agf2dbpin8xyb"))

(define rust-windows-aarch64-gnullvm-0.52.5
  (crate-source "windows_aarch64_gnullvm" "0.52.5"
                "0qrjimbj67nnyn7zqy15mzzmqg0mn5gsr2yciqjxm3cb3vbyx23h"))

(define rust-windows-aarch64-gnullvm-0.52.6
  (crate-source "windows_aarch64_gnullvm" "0.52.6"
                "1lrcq38cr2arvmz19v32qaggvj8bh1640mdm9c2fr877h0hn591j"))

(define rust-windows-aarch64-gnullvm-0.53.0
  (crate-source "windows_aarch64_gnullvm" "0.53.0"
                "0r77pbpbcf8bq4yfwpz2hpq3vns8m0yacpvs2i5cn6fx1pwxbf46"))

(define rust-windows-aarch64-gnullvm-0.53.1
  (crate-source "windows_aarch64_gnullvm" "0.53.1"
                "0lqvdm510mka9w26vmga7hbkmrw9glzc90l4gya5qbxlm1pl3n59"))

(define rust-windows-aarch64-msvc-0.36.1
  (crate-source "windows_aarch64_msvc" "0.36.1"
                "0ixaxs2c37ll2smprzh0xq5p238zn8ylzb3lk1zddqmd77yw7f4v"))

(define rust-windows-aarch64-msvc-0.42.2
  (crate-source "windows_aarch64_msvc" "0.42.2"
                "0hsdikjl5sa1fva5qskpwlxzpc5q9l909fpl1w6yy1hglrj8i3p0"))

(define rust-windows-aarch64-msvc-0.48.0
  (crate-source "windows_aarch64_msvc" "0.48.0"
                "1wvwipchhywcjaw73h998vzachf668fpqccbhrxzrz5xszh2gvxj"))

(define rust-windows-aarch64-msvc-0.48.5
  (crate-source "windows_aarch64_msvc" "0.48.5"
                "1g5l4ry968p73g6bg6jgyvy9lb8fyhcs54067yzxpcpkf44k2dfw"))

(define rust-windows-aarch64-msvc-0.52.0
  (crate-source "windows_aarch64_msvc" "0.52.0"
                "1vvmy1ypvzdvxn9yf0b8ygfl85gl2gpcyvsvqppsmlpisil07amv"))

(define rust-windows-aarch64-msvc-0.52.5
  (crate-source "windows_aarch64_msvc" "0.52.5"
                "1dmga8kqlmln2ibckk6mxc9n59vdg8ziqa2zr8awcl720hazv1cr"))

(define rust-windows-aarch64-msvc-0.52.6
  (crate-source "windows_aarch64_msvc" "0.52.6"
                "0sfl0nysnz32yyfh773hpi49b1q700ah6y7sacmjbqjjn5xjmv09"))

(define rust-windows-aarch64-msvc-0.53.0
  (crate-source "windows_aarch64_msvc" "0.53.0"
                "0v766yqw51pzxxwp203yqy39ijgjamp54hhdbsyqq6x1c8gilrf7"))

(define rust-windows-aarch64-msvc-0.53.1
  (crate-source "windows_aarch64_msvc" "0.53.1"
                "01jh2adlwx043rji888b22whx4bm8alrk3khjpik5xn20kl85mxr"))

(define rust-windows-collections-0.1.1
  (crate-source "windows-collections" "0.1.1"
                "1v2dq9dy2lm8v5c98hn3diixprxqb6sdnhgdnbmm4gxsq6fgfrsl"))

(define rust-windows-collections-0.2.0
  (crate-source "windows-collections" "0.2.0"
                "1s65anr609qvsjga7w971p6iq964h87670dkfqfypnfgwnswxviv"))

(define rust-windows-collections-0.3.2
  (crate-source "windows-collections" "0.3.2"
                "0436rjbkqn3j9m2v2lcmwwk0l3n2r57yvqb7fcy4m8d8y5ddkci3"))

(define rust-windows-core-0.52.0
  (crate-source "windows-core" "0.52.0"
                "1nc3qv7sy24x0nlnb32f7alzpd6f72l4p24vl65vydbyil669ark"))

(define rust-windows-core-0.54.0
  (crate-source "windows-core" "0.54.0"
                "0r8x2sgl4qq1h23ldf4z7cj213k0bz7479m8a156h79mi6f1nrhj"))

(define rust-windows-core-0.56.0
  (crate-source "windows-core" "0.56.0"
                "19pj57bm0rzhlk0ghrccd3i5zvh0ghm52f8cmdc8d3yhs8pfb626"))

(define rust-windows-core-0.58.0
  (crate-source "windows-core" "0.58.0"
                "16czypy425jzmiys4yb3pwsh7cm6grxn9kjp889iqnf2r17d99kb"))

(define rust-windows-core-0.60.1
  (crate-source "windows-core" "0.60.1"
                "0iv25wamzs29cy9mfvbfvw6i0dq8rs6kdy2wmv6g96xfkhmaj8fa"))

(define rust-windows-core-0.61.2
  (crate-source "windows-core" "0.61.2"
                "1qsa3iw14wk4ngfl7ipcvdf9xyq456ms7cx2i9iwf406p7fx7zf0"))

(define rust-windows-core-0.62.1
  (crate-source "windows-core" "0.62.1"
                "1aa94x61q0x39xnlzxjmahwck9i5p51xgzrz7m6hi1dj2rafwi38"))

(define rust-windows-core-0.62.2
  (crate-source "windows-core" "0.62.2"
                "1swxpv1a8qvn3bkxv8cn663238h2jccq35ff3nsj61jdsca3ms5q"))

(define rust-windows-future-0.1.1
  (crate-source "windows-future" "0.1.1"
                "1w296g00054hzqpjy1xb8gixhqqkzfmqpkklkciq1sz7jm2xp1x7"))

(define rust-windows-future-0.2.1
  (crate-source "windows-future" "0.2.1"
                "13mdzcdn51ckpzp3frb8glnmkyjr1c30ym9wnzj9zc97hkll2spw"))

(define rust-windows-future-0.3.2
  (crate-source "windows-future" "0.3.2"
                "1jq5qs2dwzf6rl60f8gr49z2mifxsrdh4y4yfdws467ya41gkmp1"))

(define rust-windows-i686-gnu-0.36.1
  (crate-source "windows_i686_gnu" "0.36.1"
                "1dm3svxfzamrv6kklyda9c3qylgwn5nwdps6p0kc9x6s077nq3hq"))

(define rust-windows-i686-gnu-0.42.2
  (crate-source "windows_i686_gnu" "0.42.2"
                "0kx866dfrby88lqs9v1vgmrkk1z6af9lhaghh5maj7d4imyr47f6"))

(define rust-windows-i686-gnu-0.48.0
  (crate-source "windows_i686_gnu" "0.48.0"
                "0hd2v9kp8fss0rzl83wzhw0s5z8q1b4875m6s1phv0yvlxi1jak2"))

(define rust-windows-i686-gnu-0.48.5
  (crate-source "windows_i686_gnu" "0.48.5"
                "0gklnglwd9ilqx7ac3cn8hbhkraqisd0n83jxzf9837nvvkiand7"))

(define rust-windows-i686-gnu-0.52.0
  (crate-source "windows_i686_gnu" "0.52.0"
                "04zkglz4p3pjsns5gbz85v4s5aw102raz4spj4b0lmm33z5kg1m2"))

(define rust-windows-i686-gnu-0.52.5
  (crate-source "windows_i686_gnu" "0.52.5"
                "0w4np3l6qwlra9s2xpflqrs60qk1pz6ahhn91rr74lvdy4y0gfl8"))

(define rust-windows-i686-gnu-0.52.6
  (crate-source "windows_i686_gnu" "0.52.6"
                "02zspglbykh1jh9pi7gn8g1f97jh1rrccni9ivmrfbl0mgamm6wf"))

(define rust-windows-i686-gnu-0.53.0
  (crate-source "windows_i686_gnu" "0.53.0"
                "1hvjc8nv95sx5vdd79fivn8bpm7i517dqyf4yvsqgwrmkmjngp61"))

(define rust-windows-i686-gnu-0.53.1
  (crate-source "windows_i686_gnu" "0.53.1"
                "18wkcm82ldyg4figcsidzwbg1pqd49jpm98crfz0j7nqd6h6s3ln"))

(define rust-windows-i686-gnullvm-0.52.5
  (crate-source "windows_i686_gnullvm" "0.52.5"
                "1s9f4gff0cixd86mw3n63rpmsm4pmr4ffndl6s7qa2h35492dx47"))

(define rust-windows-i686-gnullvm-0.52.6
  (crate-source "windows_i686_gnullvm" "0.52.6"
                "0rpdx1537mw6slcpqa0rm3qixmsb79nbhqy5fsm3q2q9ik9m5vhf"))

(define rust-windows-i686-gnullvm-0.53.0
  (crate-source "windows_i686_gnullvm" "0.53.0"
                "04df1in2k91qyf1wzizvh560bvyzq20yf68k8xa66vdzxnywrrlw"))

(define rust-windows-i686-gnullvm-0.53.1
  (crate-source "windows_i686_gnullvm" "0.53.1"
                "030qaxqc4salz6l4immfb6sykc6gmhyir9wzn2w8mxj8038mjwzs"))

(define rust-windows-i686-msvc-0.36.1
  (crate-source "windows_i686_msvc" "0.36.1"
                "097h2a7wig04wbmpi3rz1akdy4s8gslj5szsx8g2v0dj91qr3rz2"))

(define rust-windows-i686-msvc-0.42.2
  (crate-source "windows_i686_msvc" "0.42.2"
                "0q0h9m2aq1pygc199pa5jgc952qhcnf0zn688454i7v4xjv41n24"))

(define rust-windows-i686-msvc-0.48.0
  (crate-source "windows_i686_msvc" "0.48.0"
                "004fkyqv3if178xx9ksqc4qqv8sz8n72mpczsr2vy8ffckiwchj5"))

(define rust-windows-i686-msvc-0.48.5
  (crate-source "windows_i686_msvc" "0.48.5"
                "01m4rik437dl9rdf0ndnm2syh10hizvq0dajdkv2fjqcywrw4mcg"))

(define rust-windows-i686-msvc-0.52.0
  (crate-source "windows_i686_msvc" "0.52.0"
                "16kvmbvx0vr0zbgnaz6nsks9ycvfh5xp05bjrhq65kj623iyirgz"))

(define rust-windows-i686-msvc-0.52.5
  (crate-source "windows_i686_msvc" "0.52.5"
                "1gw7fklxywgpnwbwg43alb4hm0qjmx72hqrlwy5nanrxs7rjng6v"))

(define rust-windows-i686-msvc-0.52.6
  (crate-source "windows_i686_msvc" "0.52.6"
                "0rkcqmp4zzmfvrrrx01260q3xkpzi6fzi2x2pgdcdry50ny4h294"))

(define rust-windows-i686-msvc-0.53.0
  (crate-source "windows_i686_msvc" "0.53.0"
                "0pcvb25fkvqnp91z25qr5x61wyya12lx8p7nsa137cbb82ayw7sq"))

(define rust-windows-i686-msvc-0.53.1
  (crate-source "windows_i686_msvc" "0.53.1"
                "1hi6scw3mn2pbdl30ji5i4y8vvspb9b66l98kkz350pig58wfyhy"))

(define rust-windows-implement-0.56.0
  (crate-source "windows-implement" "0.56.0"
                "16rgkvlx4syqmajfdwmkcvn6nvh126wjj8sg3jvsk5fdivskbz7n"))

(define rust-windows-implement-0.58.0
  (crate-source "windows-implement" "0.58.0"
                "16spr5z65z21qyv379rv2mb1s5q2i9ibd1p2pkn0dr9qr535pg9b"))

(define rust-windows-implement-0.59.0
  (crate-source "windows-implement" "0.59.0"
                "1ha89vc4v4vabwdjkz46hdsnl2jp7ckpzw88qdca0j9g3q2pnmw3"))

(define rust-windows-implement-0.60.0
  (crate-source "windows-implement" "0.60.0"
                "0dm88k3hlaax85xkls4gf597ar4z8m5vzjjagzk910ph7b8xszx4"))

(define rust-windows-implement-0.60.1
  (crate-source "windows-implement" "0.60.1"
                "1q2lfwdqrkfzsrlshvvyr2cj7ckq4rqxj0ispzlnvyvl5bj0gczd"))

(define rust-windows-implement-0.60.2
  (crate-source "windows-implement" "0.60.2"
                "1psxhmklzcf3wjs4b8qb42qb6znvc142cb5pa74rsyxm1822wgh5"))

(define rust-windows-interface-0.56.0
  (crate-source "windows-interface" "0.56.0"
                "1k2prfxna0mw47f8gi8qhw9jfpw66bh2cqzs67sgipjfpx30b688"))

(define rust-windows-interface-0.58.0
  (crate-source "windows-interface" "0.58.0"
                "059mxmfvx3x88q74ms0qlxmj2pnidmr5mzn60hakn7f95m34qg05"))

(define rust-windows-interface-0.59.1
  (crate-source "windows-interface" "0.59.1"
                "1a4zr8740gyzzhq02xgl6vx8l669jwfby57xgf0zmkcdkyv134mx"))

(define rust-windows-interface-0.59.2
  (crate-source "windows-interface" "0.59.2"
                "19a6if8dfnazjgjw4hm0kayk9vrjclyj3iqivcaaqr39pkfx3ay0"))

(define rust-windows-interface-0.59.3
  (crate-source "windows-interface" "0.59.3"
                "0n73cwrn4247d0axrk7gjp08p34x1723483jxjxjdfkh4m56qc9z"))

(define rust-windows-link-0.1.0
  (crate-source "windows-link" "0.1.0"
                "1qr0srnkw148wbrws3726pm640h2vxgcdlxn0cxpbcg27irzvk3d"))

(define rust-windows-link-0.1.3
  (crate-source "windows-link" "0.1.3"
                "12kr1p46dbhpijr4zbwr2spfgq8i8c5x55mvvfmyl96m01cx4sjy"))

(define rust-windows-link-0.2.0
  (crate-source "windows-link" "0.2.0"
                "0r9w2z96d5phmm185aq92z54jp9h2nqisa4wgc71idxbc436rr25"))

(define rust-windows-link-0.2.1
  (crate-source "windows-link" "0.2.1"
                "1rag186yfr3xx7piv5rg8b6im2dwcf8zldiflvb22xbzwli5507h"))

(define rust-windows-numerics-0.1.1
  (crate-source "windows-numerics" "0.1.1"
                "1vgfh8qh0i4ggj32339d99rvbwrw7dq8z6r7xqn9yjgnw9aflp80"))

(define rust-windows-numerics-0.2.0
  (crate-source "windows-numerics" "0.2.0"
                "1cf2j8nbqf0hqqa7chnyid91wxsl2m131kn0vl3mqk3c0rlayl4i"))

(define rust-windows-numerics-0.3.1
  (crate-source "windows-numerics" "0.3.1"
                "09hgbg8pf89r4090yyhh9q29ppi7yyxkgmga9ascshy19a240bkf"))

(define rust-windows-registry-0.2.0
  (crate-source "windows-registry" "0.2.0"
                "1c04923fq0rbvl3z0h67xr6rh2fgwkizhclhqv0j79i0nwdh0074"))

(define rust-windows-registry-0.5.3
  (crate-source "windows-registry" "0.5.3"
                "17j9cxlnksdypanazss6cnh36v3rwvs86j4mpixwkvv5hz99x2jv"))

(define rust-windows-result-0.1.2
  (crate-source "windows-result" "0.1.2"
                "1y274q1v0vy21lhkgslpxpq1m08hvr1mcs2l88h1b1gcx0136f2y"))

(define rust-windows-result-0.2.0
  (crate-source "windows-result" "0.2.0"
                "03mf2z1xcy2slhhsm15z24p76qxgm2m74xdjp8bihyag47c4640x"))

(define rust-windows-result-0.3.1
  (crate-source "windows-result" "0.3.1"
                "12dihsnl408sjjlyairc8vwjig68dvlfc00mi17pxawghpz4wdq6"))

(define rust-windows-result-0.3.4
  (crate-source "windows-result" "0.3.4"
                "1il60l6idrc6hqsij0cal0mgva6n3w6gq4ziban8wv6c6b9jpx2n"))

(define rust-windows-result-0.4.0
  (crate-source "windows-result" "0.4.0"
                "0zqn8kmmf7y9yw9g7q6pbcg9dbry9m03fqi0b92q767q0v1xr13h"))

(define rust-windows-result-0.4.1
  (crate-source "windows-result" "0.4.1"
                "1d9yhmrmmfqh56zlj751s5wfm9a2aa7az9rd7nn5027nxa4zm0bp"))

(define rust-windows-strings-0.1.0
  (crate-source "windows-strings" "0.1.0"
                "042dxvi3133f7dyi2pgcvknwkikk47k8bddwxbq5s0l6qhjv3nac"))

(define rust-windows-strings-0.3.1
  (crate-source "windows-strings" "0.3.1"
                "06bkhkyclbfchcsv5bnhz77r290k20m15glj2xq60ra0bp64iyl7"))

(define rust-windows-strings-0.4.2
  (crate-source "windows-strings" "0.4.2"
                "0mrv3plibkla4v5kaakc2rfksdd0b14plcmidhbkcfqc78zwkrjn"))

(define rust-windows-strings-0.5.0
  (crate-source "windows-strings" "0.5.0"
                "1nld65azvms87rdm2bdm8gskwdmsswh4pxbc8babxc2klmawc63j"))

(define rust-windows-strings-0.5.1
  (crate-source "windows-strings" "0.5.1"
                "14bhng9jqv4fyl7lqjz3az7vzh8pw0w4am49fsqgcz67d67x0dvq"))

(define rust-windows-sys-0.36.1
  ;; TODO: Check bundled sources.
  (crate-source "windows-sys" "0.36.1"
                "1lmqangv0zg1l46xiq7rfnqwsx8f8m52mqbgg2mrx7x52rd1a17a"))

(define rust-windows-sys-0.42.0
  ;; TODO: Check bundled sources.
  (crate-source "windows-sys" "0.42.0"
                "19waf8aryvyq9pzk0gamgfwjycgzk4gnrazpfvv171cby0h1hgjs"))

(define rust-windows-sys-0.45.0
  ;; TODO: Check bundled sources.
  (crate-source "windows-sys" "0.45.0"
                "1l36bcqm4g89pknfp8r9rl1w4bn017q6a8qlx8viv0xjxzjkna3m"))

(define rust-windows-sys-0.48.0
  ;; TODO: Check bundled sources.
  (crate-source "windows-sys" "0.48.0"
                "1aan23v5gs7gya1lc46hqn9mdh8yph3fhxmhxlw36pn6pqc28zb7"))

(define rust-windows-sys-0.52.0
  ;; TODO: Check bundled sources.
  (crate-source "windows-sys" "0.52.0"
                "0gd3v4ji88490zgb6b5mq5zgbvwv7zx1ibn8v3x83rwcdbryaar8"))

(define rust-windows-sys-0.59.0
  ;; TODO: Check bundled sources.
  (crate-source "windows-sys" "0.59.0"
                "0fw5672ziw8b3zpmnbp9pdv1famk74f1l9fcbc3zsrzdg56vqf0y"))

(define rust-windows-sys-0.60.2
  ;; TODO: Check bundled sources.
  (crate-source "windows-sys" "0.60.2"
                "1jrbc615ihqnhjhxplr2kw7rasrskv9wj3lr80hgfd42sbj01xgj"))

(define rust-windows-sys-0.61.0
  ;; TODO: Check bundled sources.
  (crate-source "windows-sys" "0.61.0"
                "1ajpwsmzfcsa1r7i0dxzvfn24dp3525rcd7aq95ydvdj8171h0g2"))

(define rust-windows-sys-0.61.1
  ;; TODO: Check bundled sources.
  (crate-source "windows-sys" "0.61.1"
                "03vg2rxm0lyiyq64b5sm95lkg2x95sjdb0zb0y4q8g2avm0rw43g"))

(define rust-windows-sys-0.61.2
  ;; TODO: Check bundled sources.
  (crate-source "windows-sys" "0.61.2"
                "1z7k3y9b6b5h52kid57lvmvm05362zv1v8w0gc7xyv5xphlp44xf"))

(define rust-windows-targets-0.42.2
  (crate-source "windows-targets" "0.42.2"
                "0wfhnib2fisxlx8c507dbmh97kgij4r6kcxdi0f9nk6l1k080lcf"))

(define rust-windows-targets-0.48.0
  (crate-source "windows-targets" "0.48.0"
                "1mfzg94w0c8h4ya9sva7rra77f3iy1712af9b6bwg03wrpqbc7kv"))

(define rust-windows-targets-0.48.5
  (crate-source "windows-targets" "0.48.5"
                "034ljxqshifs1lan89xwpcy1hp0lhdh4b5n0d2z4fwjx2piacbws"))

(define rust-windows-targets-0.52.0
  (crate-source "windows-targets" "0.52.0"
                "1kg7a27ynzw8zz3krdgy6w5gbqcji27j1sz4p7xk2j5j8082064a"))

(define rust-windows-targets-0.52.5
  (crate-source "windows-targets" "0.52.5"
                "1sz7jrnkygmmlj1ia8fk85wbyil450kq5qkh5qh9sh2rcnj161vg"))

(define rust-windows-targets-0.52.6
  (crate-source "windows-targets" "0.52.6"
                "0wwrx625nwlfp7k93r2rra568gad1mwd888h1jwnl0vfg5r4ywlv"))

(define rust-windows-targets-0.53.2
  (crate-source "windows-targets" "0.53.2"
                "1vwanhx2br7dh8mmrszdbcf01bccjr01mcyxcscxl4ffr7y6jvy6"))

(define rust-windows-targets-0.53.3
  (crate-source "windows-targets" "0.53.3"
                "14fwwm136dhs3i1impqrrip7nvkra3bdxa4nqkblj604qhqn1znm"))

(define rust-windows-targets-0.53.4
  (crate-source "windows-targets" "0.53.4"
                "0jxc6f032xb3bbb7mj9rlhky84w7vz7hkbsh8s2hcakdysvvfhid"))

(define rust-windows-targets-0.53.5
  (crate-source "windows-targets" "0.53.5"
                "1wv9j2gv3l6wj3gkw5j1kr6ymb5q6dfc42yvydjhv3mqa7szjia9"))

(define rust-windows-threading-0.1.0
  (crate-source "windows-threading" "0.1.0"
                "19jpn37zpjj2q7pn07dpq0ay300w65qx7wdp13wbp8qf5snn6r5n"))

(define rust-windows-threading-0.2.1
  (crate-source "windows-threading" "0.2.1"
                "0dsvsy33vxs0153z4n39sqkzx382cjjkrd46rb3z3zfak5dvsj9r"))

(define rust-windows-x86-64-gnu-0.36.1
  (crate-source "windows_x86_64_gnu" "0.36.1"
                "1qfrck3jnihymfrd01s8260d4snql8ks2p8yaabipi3nhwdigkad"))

(define rust-windows-x86-64-gnu-0.42.2
  (crate-source "windows_x86_64_gnu" "0.42.2"
                "0dnbf2xnp3xrvy8v9mgs3var4zq9v9yh9kv79035rdgyp2w15scd"))

(define rust-windows-x86-64-gnu-0.48.0
  (crate-source "windows_x86_64_gnu" "0.48.0"
                "1cblz5m6a8q6ha09bz4lz233dnq5sw2hpra06k9cna3n3xk8laya"))

(define rust-windows-x86-64-gnu-0.48.5
  (crate-source "windows_x86_64_gnu" "0.48.5"
                "13kiqqcvz2vnyxzydjh73hwgigsdr2z1xpzx313kxll34nyhmm2k"))

(define rust-windows-x86-64-gnu-0.52.0
  (crate-source "windows_x86_64_gnu" "0.52.0"
                "1zdy4qn178sil5sdm63lm7f0kkcjg6gvdwmcprd2yjmwn8ns6vrx"))

(define rust-windows-x86-64-gnu-0.52.5
  (crate-source "windows_x86_64_gnu" "0.52.5"
                "1n8p2mcf3lw6300k77a0knksssmgwb9hynl793mhkzyydgvlchjf"))

(define rust-windows-x86-64-gnu-0.52.6
  (crate-source "windows_x86_64_gnu" "0.52.6"
                "0y0sifqcb56a56mvn7xjgs8g43p33mfqkd8wj1yhrgxzma05qyhl"))

(define rust-windows-x86-64-gnu-0.53.0
  (crate-source "windows_x86_64_gnu" "0.53.0"
                "1flh84xkssn1n6m1riddipydcksp2pdl45vdf70jygx3ksnbam9f"))

(define rust-windows-x86-64-gnu-0.53.1
  (crate-source "windows_x86_64_gnu" "0.53.1"
                "16d4yiysmfdlsrghndr97y57gh3kljkwhfdbcs05m1jasz6l4f4w"))

(define rust-windows-x86-64-gnullvm-0.42.2
  (crate-source "windows_x86_64_gnullvm" "0.42.2"
                "18wl9r8qbsl475j39zvawlidp1bsbinliwfymr43fibdld31pm16"))

(define rust-windows-x86-64-gnullvm-0.48.0
  (crate-source "windows_x86_64_gnullvm" "0.48.0"
                "0lxryz3ysx0145bf3i38jkr7f9nxiym8p3syklp8f20yyk0xp5kq"))

(define rust-windows-x86-64-gnullvm-0.48.5
  (crate-source "windows_x86_64_gnullvm" "0.48.5"
                "1k24810wfbgz8k48c2yknqjmiigmql6kk3knmddkv8k8g1v54yqb"))

(define rust-windows-x86-64-gnullvm-0.52.0
  (crate-source "windows_x86_64_gnullvm" "0.52.0"
                "17lllq4l2k1lqgcnw1cccphxp9vs7inq99kjlm2lfl9zklg7wr8s"))

(define rust-windows-x86-64-gnullvm-0.52.5
  (crate-source "windows_x86_64_gnullvm" "0.52.5"
                "15n56jrh4s5bz66zimavr1rmcaw6wa306myrvmbc6rydhbj9h8l5"))

(define rust-windows-x86-64-gnullvm-0.52.6
  (crate-source "windows_x86_64_gnullvm" "0.52.6"
                "03gda7zjx1qh8k9nnlgb7m3w3s1xkysg55hkd1wjch8pqhyv5m94"))

(define rust-windows-x86-64-gnullvm-0.53.0
  (crate-source "windows_x86_64_gnullvm" "0.53.0"
                "0mvc8119xpbi3q2m6mrjcdzl6afx4wffacp13v76g4jrs1fh6vha"))

(define rust-windows-x86-64-gnullvm-0.53.1
  (crate-source "windows_x86_64_gnullvm" "0.53.1"
                "1qbspgv4g3q0vygkg8rnql5c6z3caqv38japiynyivh75ng1gyhg"))

(define rust-windows-x86-64-msvc-0.36.1
  (crate-source "windows_x86_64_msvc" "0.36.1"
                "103n3xijm5vr7qxr1dps202ckfnv7njjnnfqmchg8gl5ii5cl4f8"))

(define rust-windows-x86-64-msvc-0.42.2
  (crate-source "windows_x86_64_msvc" "0.42.2"
                "1w5r0q0yzx827d10dpjza2ww0j8iajqhmb54s735hhaj66imvv4s"))

(define rust-windows-x86-64-msvc-0.48.0
  (crate-source "windows_x86_64_msvc" "0.48.0"
                "12ipr1knzj2rwjygyllfi5mkd0ihnbi3r61gag5n2jgyk5bmyl8s"))

(define rust-windows-x86-64-msvc-0.48.5
  (crate-source "windows_x86_64_msvc" "0.48.5"
                "0f4mdp895kkjh9zv8dxvn4pc10xr7839lf5pa9l0193i2pkgr57d"))

(define rust-windows-x86-64-msvc-0.52.0
  (crate-source "windows_x86_64_msvc" "0.52.0"
                "012wfq37f18c09ij5m6rniw7xxn5fcvrxbqd0wd8vgnl3hfn9yfz"))

(define rust-windows-x86-64-msvc-0.52.5
  (crate-source "windows_x86_64_msvc" "0.52.5"
                "1w1bn24ap8dp9i85s8mlg8cim2bl2368bd6qyvm0xzqvzmdpxi5y"))

(define rust-windows-x86-64-msvc-0.52.6
  (crate-source "windows_x86_64_msvc" "0.52.6"
                "1v7rb5cibyzx8vak29pdrk8nx9hycsjs4w0jgms08qk49jl6v7sq"))

(define rust-windows-x86-64-msvc-0.53.0
  (crate-source "windows_x86_64_msvc" "0.53.0"
                "11h4i28hq0zlnjcaqi2xdxr7ibnpa8djfggch9rki1zzb8qi8517"))

(define rust-windows-x86-64-msvc-0.53.1
  (crate-source "windows_x86_64_msvc" "0.53.1"
                "0l6npq76vlq4ksn4bwsncpr8508mk0gmznm6wnhjg95d19gzzfyn"))

(define rust-winit-0.30.12
  (crate-source "winit" "0.30.12"
                "0cn7wvli4s0l3v5rf6s3rn4j72mdc5p2sxhz6bv0jh4wssg4nvf6"))

(define rust-winit-0.30.7
  (crate-source "winit" "0.30.7"
                "18xiwx8qvxjwv0dkjvy5csy3zxm8m5dbjkr7r5silvsbxz40p9fv"))

(define rust-winnow-0.5.10
  (crate-source "winnow" "0.5.10"
                "0za2h82dax21jd0d82rwgj34psflkfzmajjwq35r7dgl8ivcq12m"))

(define rust-winnow-0.5.39
  (crate-source "winnow" "0.5.39"
                "0a9d8yxfnlgfd27samr5ld6hlpx7vr4qzxpqfy6d50qnn1aa32ak"))

(define rust-winnow-0.6.21
  (crate-source "winnow" "0.6.21"
                "0s3x124zh15c2q2hd9kh7skr48b9v6zlkry64ma7lh7jax9bpxg6"))

(define rust-winnow-0.6.24
  (crate-source "winnow" "0.6.24"
                "0fm0z1gk9wb47s1jhh889isz657kavd1yb3fhzbjmi657icimmy8"))

(define rust-winnow-0.6.5
  (crate-source "winnow" "0.6.5"
                "1f0m48kfdvds456b1i1qd6ia9w3mckk2jf4q6z94snpdcw741ynz"))

(define rust-winnow-0.7.12
  (crate-source "winnow" "0.7.12"
                "159y8inpy86xswmr4yig9hxss0v2fssyqy1kk12504n8jbsfpvgk"))

(define rust-winnow-0.7.13
  (crate-source "winnow" "0.7.13"
                "1krrjc1wj2vx0r57m9nwnlc1zrhga3fq41d8w9hysvvqb5mj7811"))

(define rust-winnow-0.7.14
  (crate-source "winnow" "0.7.14"
                "0a88ahjqhyn2ln1yplq2xsigm09kxqkdkkk2c2mfxkbzszln8lss"))

(define rust-winnow-0.7.3
  (crate-source "winnow" "0.7.3"
                "1c9bmhpdwbdmll6b4l6skabz0296dchnmnxw84hh2y3ggyllwzqf"))

(define rust-winnow-0.7.4
  (crate-source "winnow" "0.7.4"
                "0dmbsz6zfddcgsqzzqxw1h8f7zy19x407g7zl3hyp6vf2m2bb5qf"))

(define rust-winreg-0.10.1
  (crate-source "winreg" "0.10.1"
                "17c6h02z88ijjba02bnxi5k94q5cz490nf3njh9yypf8fbig9l40"))

(define rust-winreg-0.50.0
  (crate-source "winreg" "0.50.0"
                "1cddmp929k882mdh6i9f2as848f13qqna6czwsqzkh1pqnr5fkjj"))

(define rust-winreg-0.52.0
  (crate-source "winreg" "0.52.0"
                "19gh9vp7mp1ab84kc3ag48nm9y7xgjhh3xa4vxss1gylk1rsaxx2"))

(define rust-winreg-0.55.0
  (crate-source "winreg" "0.55.0"
                "15xy060vylrsp91bc0ximx3xziwipzlrn1n2ab19w3n56x9pcnnb"))

(define rust-winres-0.1.12
  (crate-source "winres" "0.1.12"
                "0v2gvqnd8iwwvb6fs69nv0mmk1z96430527n0qlfbsarxxhv53dn"))

(define rust-winsafe-0.0.19
  (crate-source "winsafe" "0.0.19"
                "0169xy9mjma8dys4m8v4x0xhw2gkbhv2v1wsbvcjl9bhnxxd2dfi"))

(define rust-wio-0.2.2
  (crate-source "wio" "0.2.2"
                "199p404fp96w1f1c93bf1jrvaqwypxf3hmmldhww4jk4yhr9j4jx"))

(define rust-wit-bindgen-0.41.0
  (crate-source "wit-bindgen" "0.41.0"
                "1plmsvaxnm0gq5kqqddmksi456hcyynb2zmwdgajjfcvd146dyqh"))

(define rust-wit-bindgen-0.46.0
  (crate-source "wit-bindgen" "0.46.0"
                "0ngysw50gp2wrrfxbwgp6dhw1g6sckknsn3wm7l00vaf7n48aypi"))

(define rust-wit-bindgen-0.51.0
  (crate-source "wit-bindgen" "0.51.0"
                "19fazgch8sq5cvjv3ynhhfh5d5x08jq2pkw8jfb05vbcyqcr496p"))

(define rust-wit-bindgen-core-0.41.0
  (crate-source "wit-bindgen-core" "0.41.0"
                "16xgwph86affl0jyz8y41p532yrjfd5bgj9wgzrd7xig9wfpiylj"))

(define rust-wit-bindgen-rt-0.33.0
  (crate-source "wit-bindgen-rt" "0.33.0"
                "0g4lwfp9x6a2i1hgjn8k14nr4fsnpd5izxhc75zpi2s5cvcg6s1j"))

(define rust-wit-bindgen-rt-0.39.0
  (crate-source "wit-bindgen-rt" "0.39.0"
                "1hd65pa5hp0nl664m94bg554h4zlhrzmkjsf6lsgsb7yc4734hkg"))

(define rust-wit-bindgen-rt-0.41.0
  (crate-source "wit-bindgen-rt" "0.41.0"
                "08an1kjpdkx2bp18iv8wxdj6bvllaw2n9h4ly5chmysd3nhm5ny4"))

(define rust-wit-bindgen-rust-0.41.0
  (crate-source "wit-bindgen-rust" "0.41.0"
                "1kpkbw48993bma1p0sispyjkqnnwmp02zwqvcsc2x7m1bgf0j24x"))

(define rust-wit-bindgen-rust-macro-0.41.0
  (crate-source "wit-bindgen-rust-macro" "0.41.0"
                "16cpnjfzamwlpxpky7n8a75pdnp57s02ln8r0z308klh2z0fw6dd"))

(define rust-wit-component-0.227.1
  (crate-source "wit-component" "0.227.1"
                "0xmnhvgl3a38hfjd7vixsg49qrhrldrzn5qs6krcn8jlb7f3lp33"))

(define rust-wit-parser-0.227.1
  (crate-source "wit-parser" "0.227.1"
                "04gwwa6av1hqsw274zk5s7a2822a2a617yanmx5nw12pa7nlbx6x"))

(define rust-write16-1.0.0
  (crate-source "write16" "1.0.0"
                "0dnryvrrbrnl7vvf5vb1zkmwldhjkf2n5znliviam7bm4900z2fi"))

(define rust-writeable-0.5.5
  (crate-source "writeable" "0.5.5"
                "0lawr6y0bwqfyayf3z8zmqlhpnzhdx0ahs54isacbhyjwa7g778y"))

(define rust-writeable-0.6.1
  (crate-source "writeable" "0.6.1"
                "1fx29zncvbrqzgz7li88vzdm8zvgwgwy2r9bnjqxya09pfwi0bza"))

(define rust-writeable-0.6.2
  (crate-source "writeable" "0.6.2"
                "1fg08y97n6vk7l0rnjggw3xyrii6dcqg54wqaxldrlk98zdy1pcy"))

(define rust-wyz-0.5.1
  (crate-source "wyz" "0.5.1"
                "1vdrfy7i2bznnzjdl9vvrzljvs4s3qm8bnlgqwln6a941gy61wq5"))

(define rust-x11-clipboard-0.8.1
  (crate-source "x11-clipboard" "0.8.1"
                "1ps0fk1912vzy382fc8l926q8w1l8bxmw72l3kr9bwdi2l8wl6ml"))

(define rust-x11-clipboard-0.9.3
  (crate-source "x11-clipboard" "0.9.3"
                "18rmsm0lrcc4hy2wqs7gn90czv59sv5bj07bbf76nfbysyrp8bb6"))

(define rust-x11-dl-2.21.0
  (crate-source "x11-dl" "2.21.0"
                "0vsiq62xpcfm0kn9zjw5c9iycvccxl22jya8wnk18lyxzqj5jwrq"))

(define rust-x11rb-0.12.0
  (crate-source "x11rb" "0.12.0"
                "02h492k920mb1733cdmly138zfiwkspil6ssqcvi7inyshk1nr5i"))

(define rust-x11rb-0.13.1
  (crate-source "x11rb" "0.13.1"
                "04jyfm0xmc538v09pzsyr2w801yadsgvyl2p0p76hzzffg5gz4ax"))

(define rust-x11rb-0.13.2
  (crate-source "x11rb" "0.13.2"
                "053lvnaw9ycbl791mgwly2hw27q6vqgzrb1y5kz1as52wmdsm4wr"))

(define rust-x11rb-protocol-0.12.0
  (crate-source "x11rb-protocol" "0.12.0"
                "1g24qdvq0mbyl2npz7zqy5v6hpdxq2qakkpnp3x02rzvl3ww7ml2"))

(define rust-x11rb-protocol-0.13.1
  (crate-source "x11rb-protocol" "0.13.1"
                "0gfbxf2k7kbk577j3rjhfx7hm70kmwln6da7xyc4l2za0d2pq47c"))

(define rust-x11rb-protocol-0.13.2
  (crate-source "x11rb-protocol" "0.13.2"
                "1g81cznbyn522b0fbis0i44wh3adad2vhsz5pzf99waf3sbc4vza"))

(define rust-x25519-dalek-2.0.1
  (crate-source "x25519-dalek" "2.0.1"
                "0xyjgqpsa0q6pprakdp58q1hy45rf8wnqqscgzx0gyw13hr6ir67"))

(define rust-x509-parser-0.18.0
  (crate-source "x509-parser" "0.18.0"
                "09b45c4bxl9i2lgxbc64lqqk7qrw02ghgllqd09nanhi21ri6gpb"))

(define rust-xcursor-0.3.10
  (crate-source "xcursor" "0.3.10"
                "0awgy98awg4ydcfmynqfcwvl4bnnfcm4i2vvnk2n926a02jy9jdy"))

(define rust-xcursor-0.3.4
  (crate-source "xcursor" "0.3.4"
                "1dwh8vkll79i2vr56dmn9fdpw2ig0klcb0a3300l7k8k6fk0ads6"))

(define rust-xcursor-0.3.8
  (crate-source "xcursor" "0.3.8"
                "0qazsl7h8nrbbzx84qrv39w8m2qc27g0mvrszgdls2v6n6k3vwqf"))

(define rust-xdg-2.5.2
  (crate-source "xdg" "2.5.2"
                "0im5nzmywxjgm2pmb48k0cc9hkalarz57f1d9d0x4lvb6cj76fr1"))

(define rust-xdg-home-1.0.0
  (crate-source "xdg-home" "1.0.0"
                "1kbd1ks8bvpsay6lgk60yaf1w13daaf75ghmslan031ss4y20s97"))

(define rust-xdg-home-1.3.0
  (crate-source "xdg-home" "1.3.0"
                "1xm122zz0wjc8p8cmchij0j9nw34hwncb39jc7dc0mgvb2rdl77c"))

(define rust-xkbcommon-dl-0.4.2
  (crate-source "xkbcommon-dl" "0.4.2"
                "1iai0r3b5skd9vbr8z5b0qixiz8jblzfm778ddm8ba596a0dwffh"))

(define rust-xkeysym-0.2.0
  (crate-source "xkeysym" "0.2.0"
                "0886dn1rlkiazcp5n6ayqfg0ibpiny62dlbiyr9v4l32nxl8wjh5"))

(define rust-xkeysym-0.2.1
  (crate-source "xkeysym" "0.2.1"
                "0mksx670cszyd7jln6s7dhkw11hdfv7blwwr3isq98k22ljh1k5r"))

(define rust-xml-rs-0.8.24
  (crate-source "xml-rs" "0.8.24"
                "0ck4s3j1dk4bnqam9h06nfmicnzdp69pz7y2hihlj2vrk8f3k2za"))

(define rust-xml-rs-0.8.25
  (crate-source "xml-rs" "0.8.25"
                "1i73ajf6scni5bi1a51r19xykgrambdx5fkks0fyg5jqqbml1ff5"))

(define rust-xml-rs-0.8.27
  (crate-source "xml-rs" "0.8.27"
                "1irplg223x6w3lvj0yig6czbiwci06495wc9xg3660kh6cvl1n3g"))

(define rust-xml-rs-0.8.28
  (crate-source "xml-rs" "0.8.28"
                "0grdj7xwbki5zrkalrg8dljyf14y4yj3wrj34sbzqp06i9zk7s1s"))

(define rust-xmlwriter-0.1.0
  (crate-source "xmlwriter" "0.1.0"
                "1fg0ldmkgiis6hnxpi1c9gy7v23y0lpi824bp8yp12fi3r82lypc"))

(define rust-xterm-query-0.5.2
  (crate-source "xterm-query" "0.5.2"
                "0cak14nd70qd99vrrikzaqlks6k8l7gyrbx7hz6lxpjg8ggk6b19"))

(define rust-yaml-rust-0.4.5
  (crate-source "yaml-rust" "0.4.5"
                "118wbqrr4n6wgk5rjjnlrdlahawlxc1bdsx146mwk8f79in97han"))

(define rust-yaml-rust2-0.10.4
  (crate-source "yaml-rust2" "0.10.4"
                "1n8yc8biqq4ffkpaycxqa3kr0avzg09jw19xg7c9cm24kh1ylqi4"))

(define rust-yansi-0.5.1
  (crate-source "yansi" "0.5.1"
                "1v4qljgzh73knr7291cgwrf56zrvhmpn837n5n5pypzq1kciq109"))

(define rust-yansi-1.0.1
  (crate-source "yansi" "1.0.1"
                "0jdh55jyv0dpd38ij4qh60zglbw9aa8wafqai6m0wa7xaxk3mrfg"))

(define rust-yasna-0.5.2
  (crate-source "yasna" "0.5.2"
                "1ka4ixrplnrfqyl1kymdj8cwpdp2k0kdr73b57hilcn1kiab6yz1"))

(define rust-yazi-0.1.6
  (crate-source "yazi" "0.1.6"
                "1qcbv74q8giikn160lfaij5kj0ki3ava5a6p4dg5ncqkjnn52i69"))

(define rust-yazi-0.2.1
  (crate-source "yazi" "0.2.1"
                "1dgv0dd0329dcbs57z2wj03daz8y1axgprw3pf5yf5jsbcjkh5z0"))

(define rust-yeslogic-fontconfig-sys-6.0.0
  ;; TODO: Check bundled sources.
  (crate-source "yeslogic-fontconfig-sys" "6.0.0"
                "1gcx4brx8xvdpvryixk32b3xn9v80ilvi5frd40l8z039imhcfjh"))

(define rust-yoke-0.7.5
  (crate-source "yoke" "0.7.5"
                "0h3znzrdmll0a7sglzf9ji0p5iqml11wrj1dypaf6ad6kbpnl3hj"))

(define rust-yoke-0.8.0
  (crate-source "yoke" "0.8.0"
                "1k4mfr48vgi7wh066y11b7v1ilakghlnlhw9snzz8vi2p00vnhaz"))

(define rust-yoke-0.8.1
  (crate-source "yoke" "0.8.1"
                "0m29dm0bf5iakxgma0bj6dbmc3b8qi9b1vaw9sa76kdqmz3fbmkj"))

(define rust-yoke-derive-0.7.5
  (crate-source "yoke-derive" "0.7.5"
                "0m4i4a7gy826bfvnqa9wy6sp90qf0as3wps3wb0smjaamn68g013"))

(define rust-yoke-derive-0.8.0
  (crate-source "yoke-derive" "0.8.0"
                "1dha5jrjz9jaq8kmxq1aag86b98zbnm9lyjrihy5sv716sbkrniq"))

(define rust-yoke-derive-0.8.1
  (crate-source "yoke-derive" "0.8.1"
                "0pbyja133jnng4mrhimzdq4a0y26421g734ybgz8wsgbfhl0andn"))

(define rust-z-base-32-0.1.4
  (crate-source "z-base-32" "0.1.4"
                "0fqk028mzvgnx9phqzvhzrx3fbzv4r736cm3x3hid136g157pgr1"))

(define rust-zbus-3.15.2
  (crate-source "zbus" "3.15.2"
                "1ri5gklhh3kl9gywym95679xs7n3sw2j3ky80jcd8siacc5ifpb7"))

(define rust-zbus-4.4.0
  (crate-source "zbus" "4.4.0"
                "09f7916lp7haxv1y5zgcg99ny15whi6dn3waf1afcafxx8mh35xv"))

(define rust-zbus-5.12.0
  (crate-source "zbus" "5.12.0"
                "14bcmn5lh0fidsg3yyxvkvxhhkm4sv90336ws8f3vagpan0v28mn"))

(define rust-zbus-5.2.0
  (crate-source "zbus" "5.2.0"
                "18f8w8cc6v88nrp0cdz57rnnh665iw6vlzl52kxvd11plkdylrzv"))

(define rust-zbus-macros-3.15.2
  (crate-source "zbus_macros" "3.15.2"
                "19g0d7d4b8l8ycw498sz8pwkplv300j31i9hnihq0zl81xxljcbi"))

(define rust-zbus-macros-4.4.0
  (crate-source "zbus_macros" "4.4.0"
                "0glqn6ddgv4ra734p343a41rrxb0phy1v13dljzhpsc1f10bjz96"))

(define rust-zbus-macros-5.12.0
  (crate-source "zbus_macros" "5.12.0"
                "0553nr19xi0sapjgd6ax25kqnahfv35x3dcqqalpra583j199nqw"))

(define rust-zbus-macros-5.2.0
  (crate-source "zbus_macros" "5.2.0"
                "0mwcab3fwfabi3bz0akgk3k5v5ys0ichmr5by9hbdkk0r7mlk79c"))

(define rust-zbus-names-2.6.0
  (crate-source "zbus_names" "2.6.0"
                "1adl0acl39flk4jnrv7axg29kpwm7cy15w2wf0yy59mxdmvvp07v"))

(define rust-zbus-names-3.0.0
  (crate-source "zbus_names" "3.0.0"
                "0v1f0ajwafj47bf11yp0xdgp26r93lslr9nb2v6624h2gppiz6sb"))

(define rust-zbus-names-4.1.0
  (crate-source "zbus_names" "4.1.0"
                "0yv4rqd0hrky4iqb6gzmkwwwibncigpcwmkqsipq8w8zh4w7lsw5"))

(define rust-zbus-names-4.2.0
  (crate-source "zbus_names" "4.2.0"
                "15ybdd6zic7simi9wjg0ywrxfq4sxg3z0wiyysadps3cpxj8xrkv"))

(define rust-zed-extension-api-0.7.0
  (crate-source "zed_extension_api" "0.7.0"
                "187wcsin3ii4li8i95jlyrzdk501rbik5ghbb67f59x09h5xaa87"))

(define rust-zeno-0.2.3
  (crate-source "zeno" "0.2.3"
                "15z6l48wv4bhsrdvjxmnr3jxjmahkrz8qjg78n9gsrmrvghgh5fx"))

(define rust-zeno-0.3.3
  (crate-source "zeno" "0.3.3"
                "0915lg3b1qiixizic9kwj2bw42x35baxqvl9dn0m2plkj91drwvd"))

(define rust-zerocopy-0.7.35
  (crate-source "zerocopy" "0.7.35"
                "1w36q7b9il2flg0qskapgi9ymgg7p985vniqd09vi0mwib8lz6qv"))

(define rust-zerocopy-0.8.26
  (crate-source "zerocopy" "0.8.26"
                "0bvsj0qzq26zc6nlrm3z10ihvjspyngs7n0jw1fz031i7h6xsf8h"))

(define rust-zerocopy-0.8.27
  (crate-source "zerocopy" "0.8.27"
                "0b1870gf2zzlckca69v2k4mqwmf8yh2li37qldnzvvd3by58g508"))

(define rust-zerocopy-0.8.31
  (crate-source "zerocopy" "0.8.31"
                "1hwqn8f0zd8h1a7qz2hxym4iaqyzk8kdxgalllydn2i5p6cfqx7x"))

(define rust-zerocopy-0.8.33
  (crate-source "zerocopy" "0.8.33"
                "1z9d6z8p1ndf0yrvw99jr5zcjnd4270kv4rivqqyi7hbs5l533v6"))

(define rust-zerocopy-derive-0.7.35
  (crate-source "zerocopy-derive" "0.7.35"
                "0gnf2ap2y92nwdalzz3x7142f2b83sni66l39vxp2ijd6j080kzs"))

(define rust-zerocopy-derive-0.8.26
  (crate-source "zerocopy-derive" "0.8.26"
                "10aiywi5qkha0mpsnb1zjwi44wl2rhdncaf3ykbp4i9nqm65pkwy"))

(define rust-zerocopy-derive-0.8.27
  (crate-source "zerocopy-derive" "0.8.27"
                "0c9qrylm2p55dvaplxsl24ma48add9qk4y0d6kjbkllaqvcvill8"))

(define rust-zerocopy-derive-0.8.31
  (crate-source "zerocopy-derive" "0.8.31"
                "0sjw20qqxbax8z8k9ifcmwjjlljjddpm0nmvih9zap7lzl4x5a6q"))

(define rust-zerocopy-derive-0.8.33
  (crate-source "zerocopy-derive" "0.8.33"
                "1wbh4bil3kqfmiwxlpzhxba6fyh09nsy87k7idk8b1hadfr64y9c"))

(define rust-zerofrom-0.1.5
  (crate-source "zerofrom" "0.1.5"
                "0bnd8vjcllzrvr3wvn8x14k2hkrpyy1fm3crkn2y3plmr44fxwyg"))

(define rust-zerofrom-0.1.6
  (crate-source "zerofrom" "0.1.6"
                "19dyky67zkjichsb7ykhv0aqws3q0jfvzww76l66c19y6gh45k2h"))

(define rust-zerofrom-derive-0.1.5
  (crate-source "zerofrom-derive" "0.1.5"
                "022q55phhb44qbrcfbc48k0b741fl8gnazw3hpmmndbx5ycfspjr"))

(define rust-zerofrom-derive-0.1.6
  (crate-source "zerofrom-derive" "0.1.6"
                "00l5niw7c1b0lf1vhvajpjmcnbdp2vn96jg4nmkhq2db0rp5s7np"))

(define rust-zeroize-1.7.0
  (crate-source "zeroize" "1.7.0"
                "0bfvby7k9pdp6623p98yz2irqnamcyzpn7zh20nqmdn68b0lwnsj"))

(define rust-zeroize-1.8.1
  (crate-source "zeroize" "1.8.1"
                "1pjdrmjwmszpxfd7r860jx54cyk94qk59x13sc307cvr5256glyf"))

(define rust-zeroize-1.8.2
  (crate-source "zeroize" "1.8.2"
                "1l48zxgcv34d7kjskr610zqsm6j2b4fcr2vfh9jm9j1jgvk58wdr"))

(define rust-zeroize-derive-1.4.2
  (crate-source "zeroize_derive" "1.4.2"
                "0sczjlqjdmrp3wn62g7mw6p438c9j4jgp2f9zamd56991mdycdnf"))

(define rust-zerotrie-0.2.2
  (crate-source "zerotrie" "0.2.2"
                "15gmka7vw5k0d24s0vxgymr2j6zn2iwl12wpmpnpjgsqg3abpw1n"))

(define rust-zerotrie-0.2.3
  (crate-source "zerotrie" "0.2.3"
                "0lbqznlqazmrwwzslw0ci7p3pqxykrbfhq29npj0gmb2amxc2n9a"))

(define rust-zerovec-0.10.4
  (crate-source "zerovec" "0.10.4"
                "0yghix7n3fjfdppwghknzvx9v8cf826h2qal5nqvy8yzg4yqjaxa"))

(define rust-zerovec-0.11.4
  (crate-source "zerovec" "0.11.4"
                "0fz7j1ns8d86m2fqg2a4bzi5gnh5892bxv4kcr9apwc6a3ajpap7"))

(define rust-zerovec-0.11.5
  (crate-source "zerovec" "0.11.5"
                "00m0p47k2g9mkv505hky5xh3r6ps7v8qc0dy4pspg542jj972a3c"))

(define rust-zerovec-derive-0.10.3
  (crate-source "zerovec-derive" "0.10.3"
                "1ik322dys6wnap5d3gcsn09azmssq466xryn5czfm13mn7gsdbvf"))

(define rust-zerovec-derive-0.11.1
  (crate-source "zerovec-derive" "0.11.1"
                "13zms8hj7vzpfswypwggyfr4ckmyc7v3di49pmj8r1qcz9z275jv"))

(define rust-zerovec-derive-0.11.2
  (crate-source "zerovec-derive" "0.11.2"
                "1wsig4h5j7a1scd5hrlnragnazjny9qjc44hancb6p6a76ay7p7a"))

(define rust-zigzag-0.1.0
  (crate-source "zigzag" "0.1.0"
                "1przksgh335b16mqf1clgi6yxnypdyw67f1hlcbcx1ldl80h9d3h"))

(define rust-zlib-rs-0.5.2
  (crate-source "zlib-rs" "0.5.2"
                "1wh0brb3cci6ifdwwz6xasznlrgb8pr99l1z8i15qpigyj9aw1ig"))

(define rust-zmij-1.0.14
  (crate-source "zmij" "1.0.14"
                "1siglzwmyqs0zc0crnm8r1mylnhv410qxdmchzw2ips8p183z3xx"))

(define rust-zmij-1.0.15
  (crate-source "zmij" "1.0.15"
                "1lh3l2yqwdylg4mkn9y9pmpmng34g2k328d6km8c3qsg3w2krxll"))

(define rust-zstd-0.13.3
  (crate-source "zstd" "0.13.3"
                "12n0h4w9l526li7jl972rxpyf012jw3nwmji2qbjghv9ll8y67p9"))

(define rust-zstd-safe-7.2.4
  (crate-source "zstd-safe" "7.2.4"
                "179vxmkzhpz6cq6mfzvgwc99bpgllkr6lwxq7ylh5dmby3aw8jcg"))

(define rust-zstd-sys-2.0.16+zstd.1.5.7
  ;; TODO: Check bundled sources.
  (crate-source "zstd-sys" "2.0.16+zstd.1.5.7"
                "0j1pd2iaqpvaxlgqmmijj68wma7xwdv9grrr63j873yw5ay9xqci"))

(define rust-zune-bmp-0.4.0
  (crate-source "zune-bmp" "0.4.0"
                "03wcrvc3a5s2154bdp07mrsxghjpmc2nabghzh1a4jrlhj6ns79v"))

(define rust-zune-core-0.4.12
  (crate-source "zune-core" "0.4.12"
                "0jj1ra86klzlcj9aha9als9d1dzs7pqv3azs1j3n96822wn3lhiz"))

(define rust-zune-core-0.5.0
  (crate-source "zune-core" "0.5.0"
                "0wr7kmnrxz0fzl7lqr5hi10fw0pwsraf4i1ivwaxfpzh42c7s7qi"))

(define rust-zune-farbfeld-0.4.0
  (crate-source "zune-farbfeld" "0.4.0"
                "04gh23s72hz8gz80phk0ljf427ksi4hj8qx88hh5d72lh3569a7f"))

(define rust-zune-hdr-0.4.0
  (crate-source "zune-hdr" "0.4.0"
                "09k4d043n7dj99x5zy3arpkjvv82hyvvminvqspi82zq6534gzsi"))

(define rust-zune-image-0.4.15
  (crate-source "zune-image" "0.4.15"
                "0q829p18hrpqm04wr27zgrdvi6274qzv7xw8dr051in81yn9hsrx"))

(define rust-zune-inflate-0.2.54
  (crate-source "zune-inflate" "0.2.54"
                "00kg24jh3zqa3i6rg6yksnb71bch9yi1casqydl00s7nw8pk7avk"))

(define rust-zune-jpeg-0.4.20
  (crate-source "zune-jpeg" "0.4.20"
                "12fhlnyr254d3hys1xkrgk5qb5sqydamy76p7kdb57p7bhh7w7zw"))

(define rust-zune-jpeg-0.4.21
  (crate-source "zune-jpeg" "0.4.21"
                "04r7g6y9jp7d4c9bq23rz3gwzlr1dsl7vdk4yly35bc4jf52rki9"))

(define rust-zune-jpeg-0.5.5
  (crate-source "zune-jpeg" "0.5.5"
                "13nm6gim4rv65bsva8ag0njng9dk701kcmzpndzs1s9j7rqbfvyw"))

(define rust-zune-jpegxl-0.4.0
  (crate-source "zune-jpegxl" "0.4.0"
                "0x1nkrxil53d3nxayflc3wzdd1m17zdvys27xncmzcc48d4fxzvi"))

(define rust-zune-png-0.4.10
  (crate-source "zune-png" "0.4.10"
                "02bmqybbma3b94m9sy6wbwvikh0a2a9z1458zvpjjvwwfs2w0abx"))

(define rust-zune-ppm-0.4.0
  (crate-source "zune-ppm" "0.4.0"
                "0x179gpzf85qkrw0dy81bgqm43z6asqv8xm51aqk8fry4d2yc9q5"))

(define rust-zune-psd-0.4.0
  (crate-source "zune-psd" "0.4.0"
                "0jadhgyjk0i3s5v347671wy77c7a8z34w3y2limj04qqc9fjf4db"))

(define rust-zune-qoi-0.4.10
  (crate-source "zune-qoi" "0.4.10"
                "1jg9hhj15zjwc8ycrmanl5an1d43vf6ps1xm7i0ldrgd55kqnnfw"))

(define rust-zvariant-3.15.0
  (crate-source "zvariant" "3.15.0"
                "131kaczf10psc8clzlndcc52dym5vi4g22kqf0qwaq6rw2z93cj4"))

(define rust-zvariant-4.2.0
  (crate-source "zvariant" "4.2.0"
                "1zl1ika7zd9bxkd0bqc78h9bykvk6xc98965iz1p3i51p452k110"))

(define rust-zvariant-5.1.0
  (crate-source "zvariant" "5.1.0"
                "0vx5w4a2s21y9agr1y9lbm8maj3rli4sjmg42aiybw9jmkk0w851"))

(define rust-zvariant-5.5.3
  (crate-source "zvariant" "5.5.3"
                "1cgzkngj9nsmz5ql9fxiqm98s0sc9l3zkr0xl9iyx4z3fmpphc4x"))

(define rust-zvariant-5.8.0
  (crate-source "zvariant" "5.8.0"
                "0k3lp0f54jblzsk47yifnqk1hjk6c85664dy4wkpgcgjwj91irib"))

(define rust-zvariant-derive-3.15.0
  (crate-source "zvariant_derive" "3.15.0"
                "1kcfgpqshggr5v7dwldjggix79zcyj7fizw7dkh6w39iziyplkck"))

(define rust-zvariant-derive-4.2.0
  (crate-source "zvariant_derive" "4.2.0"
                "0jf408h0s83krxwm7wl62fnssin1kcklmb1bcn83ls6sddabmqkk"))

(define rust-zvariant-derive-5.1.0
  (crate-source "zvariant_derive" "5.1.0"
                "05l9m8ypcf28v3xjwkl7zc2bz6hlgp976v6kpd7i1jg6zabknzk8"))

(define rust-zvariant-derive-5.5.3
  (crate-source "zvariant_derive" "5.5.3"
                "104m9pp4821mz2q6rgqnjq79q89j0iiv25w1sk63bms2rl1agzbm"))

(define rust-zvariant-derive-5.8.0
  (crate-source "zvariant_derive" "5.8.0"
                "01i05n35wrahcvw63ybradsb53g6wbc5kv5i2djpc81b3dd5fn6s"))

(define rust-zvariant-utils-1.0.1
  (crate-source "zvariant_utils" "1.0.1"
                "00625h3240rixvfhq6yhws1d4bwf3vrf74v8s69b97aq27cg0d3j"))

(define rust-zvariant-utils-2.1.0
  (crate-source "zvariant_utils" "2.1.0"
                "0h43h3jcw8rmjr390rdqnhkb9nn3913pgkvb75am1frxrkvwy6y5"))

(define rust-zvariant-utils-3.0.2
  (crate-source "zvariant_utils" "3.0.2"
                "1in90pg2x1m0jyxdrw17gzmshhicbvzyxk6cbw73c4lglc8x1l90"))

(define rust-zvariant-utils-3.2.0
  (crate-source "zvariant_utils" "3.2.0"
                "0d7wllndiv7vwgapmji3q9sxmzbaqfdxjwkqnx9vbmz58gpdyvp1"))

(define rust-zvariant-utils-3.2.1
  (crate-source "zvariant_utils" "3.2.1"
                "16g5id3h9q85c5vcwdfwkwmwzyladbr2q8x2xinr3xl95wa9v566"))

(define rust-atomic-refcell-0.1.13
  (crate-source "atomic_refcell" "0.1.13"
                "0z04ng59y22mwf315wamx78ybhjag0x6k7isc36hdgcv63c7rrj1"))

(define rust-bitflags-2.9.3
  (crate-source "bitflags" "2.9.3"
                "0pgjwsd9qgdmsmwpvg47p9ccrsc26yfjqawbhsi9qds5sg6brvrl"))

(define rust-cairo-rs-0.21.1
  (crate-source "cairo-rs" "0.21.1"
                "044gc8k024ypnvnqqhbp7cgl7qq02hwvbi9n5fpajmdpswkg6n0i"))

(define rust-cairo-sys-rs-0.21.1
  (crate-source "cairo-sys-rs" "0.21.1"
                "1hv2bkrqxcidk7wdy9giy4ivgh8fdkhrkvg54wwpi3pc01wifqxr"))

(define rust-cc-1.2.34
  (crate-source "cc" "1.2.34"
                "1p5ycww65h7xca03lwdp264qalw8v357rg5h17s7naq3h3m4mg22"))

(define rust-cfg-expr-0.20.2
  (crate-source "cfg-expr" "0.20.2"
                "01l25g563n04ci6cjv0509d8ig91djvwidx9iln4h3qg7zb5im68"))

(define rust-chrono-0.4.41
  (crate-source "chrono" "0.4.41"
                "0k8wy2mph0mgipq28vv3wirivhb31pqs7jyid0dzjivz0i9djsf4"))

(define rust-deluxe-0.5.0
  (crate-source "deluxe" "0.5.0"
                "120lbwqca3pv935882n7hd5yzqr3lgnd8gfgi845kd2jyym35lwf"))

(define rust-deluxe-core-0.5.0
  (crate-source "deluxe-core" "0.5.0"
                "04ykzz8cp28gsb3qm2s5r69b0hihyqgka325m3bgjvapr18xmnpd"))

(define rust-deluxe-macros-0.5.0
  (crate-source "deluxe-macros" "0.5.0"
                "05wi3ns9lp1l7hislxhv2hiw5hsjmrpbvf27gravgw1pr3clcxgq"))

(define rust-gdk-pixbuf-0.21.1
  (crate-source "gdk-pixbuf" "0.21.1"
                "0v2z4jq3mlf5q73p8ggkrbza11g9b6fkvbii2d1xyly6pg6k0wrw"))

(define rust-gdk-pixbuf-sys-0.21.1
  ;; TODO: Check bundled sources.
  (crate-source "gdk-pixbuf-sys" "0.21.1"
                "10ma22ym76xjziiab44z84nkvx9hny9wggifj6x8rhhxjg69jn72"))

(define rust-gdk4-0.10.0
  (crate-source "gdk4" "0.10.0"
                "0ng3lkm31idb1qq47330s9apb5vgylj7fp256a968ppks9jb0rqa"))

(define rust-gdk4-sys-0.10.0
  ;; TODO: Check bundled sources.
  (crate-source "gdk4-sys" "0.10.0"
                "1idjdn6f6ss1xxcf6xh7l42ahw7dj6jkv6mlplbm7f4yhw6xmnrf"))

(define rust-gdk4-wayland-0.10.0
  (crate-source "gdk4-wayland" "0.10.0"
                "0xlvx9prhkx3bwz0dkky2iw80xn3wsycr0zbxjam9z65hq706kph"))

(define rust-gdk4-wayland-sys-0.10.0
  ;; TODO: Check bundled sources.
  (crate-source "gdk4-wayland-sys" "0.10.0"
                "0s4346mqbrn7hrwbk0f7xyzhnjc3clh9rsrs6c3i5jj82jcabp0m"))

(define rust-gdk4-win32-0.10.0
  (crate-source "gdk4-win32" "0.10.0"
                "15kwplq5da4h41cr3y2vdfkx3z3nq498qy68v6r5pzvb0nq4gjcc"))

(define rust-gdk4-win32-sys-0.10.0
  ;; TODO: Check bundled sources.
  (crate-source "gdk4-win32-sys" "0.10.0"
                "15pyjik5da39x7hbw9xj6gzsc26psryinmz8s4ccmkfn2d34gjzv"))

(define rust-gdk4-x11-0.10.0
  (crate-source "gdk4-x11" "0.10.0"
                "1g1z6srl535fxsk0nhicd6k2xsp1a6aw4kks9mjfccyb3kas6d06"))

(define rust-gdk4-x11-sys-0.10.0
  ;; TODO: Check bundled sources.
  (crate-source "gdk4-x11-sys" "0.10.0"
                "0l63hg4616mpnz45pr9qckyvadi2rrs9rdiij0yzjcf5mj5271z3"))

(define rust-gettext-rs-0.7.2
  (crate-source "gettext-rs" "0.7.2"
                "12ikrzvx35aybip55ib9zmnjf8is4mhy2pfmgv50lhq8vkvr4km4"))

(define rust-gettext-sys-0.22.5
  ;; TODO: Check bundled sources.
  (crate-source "gettext-sys" "0.22.5"
                "0qgnkr3gaf6pndai26ysmhyr8kwn8psmhifhxhmg2ic9bczpfidv"))

(define rust-gio-0.21.1
  (crate-source "gio" "0.21.1"
                "0pscn8lk5z61ysmy6n0nx0nsrlbww80dsld41pipj6yhj3ry7daj"))

(define rust-gio-sys-0.21.1
  ;; TODO: Check bundled sources.
  (crate-source "gio-sys" "0.21.1"
                "1zcrdp8f172ahb6fj6rmx9r5fgqh5jrc4n8zzs65hnhycws24gx0"))

(define rust-glib-0.21.1
  (crate-source "glib" "0.21.1"
                "01ajrf0yfq6ksyjfzyqbaydddc3smz5zf0ckpaa5fyr5jdjc5gb0"))

(define rust-glib-macros-0.21.0
  (crate-source "glib-macros" "0.21.0"
                "1ygh1q0nnyvr000yh3764m7qk97lc8gp8xdv27mjhk51pqg2jwp7"))

(define rust-glib-sys-0.21.1
  ;; TODO: Check bundled sources.
  (crate-source "glib-sys" "0.21.1"
                "0bb2pybv7ljjd1mii8kgf7baqdwl75rg3ss53s147p57yv7l6z6w"))

(define rust-gobject-sys-0.21.1
  ;; TODO: Check bundled sources.
  (crate-source "gobject-sys" "0.21.1"
                "0p1d0vwkh0nn8shk62lf74n0dikld4q4whx8d9519kibxw71k6iy"))

(define rust-graphene-rs-0.21.1
  (crate-source "graphene-rs" "0.21.1"
                "11aacll6bvv2js6rsg70i0jkpmdgjdi9f8ww4zw4vh348hwi8sfr"))

(define rust-graphene-sys-0.21.1
  ;; TODO: Check bundled sources.
  (crate-source "graphene-sys" "0.21.1"
                "0aj2m7j89wfw251w8qk3r73g5cr3dd6wkqwv0p7l385p36xhb0ng"))

(define rust-gsettings-macro-0.2.2
  (crate-source "gsettings-macro" "0.2.2"
                "0qvjjiq03a2qdas4b8zlr25psf873zzrrv01xnkw564lrakkhfrz"))

(define rust-gsk4-0.10.0
  (crate-source "gsk4" "0.10.0"
                "0371657hjikib80swdz4k9b4lnigadpd60vwyvg21z6nxqyf7nym"))

(define rust-gsk4-sys-0.10.0
  ;; TODO: Check bundled sources.
  (crate-source "gsk4-sys" "0.10.0"
                "1jcz1fy5j4m912yagkqq52afrrkshw4fxvgxdzhzs6fxalfh2xld"))

(define rust-gst-plugin-gif-0.14.0
  (crate-source "gst-plugin-gif" "0.14.0"
                "035skrav9zg99c5v9qvr2a9wic9p2wmk3zgq9ckcba98qrqh51y1"))

(define rust-gst-plugin-gtk4-0.14.0
  (crate-source "gst-plugin-gtk4" "0.14.0"
                "0lxq7c1lzk6jhxy6fv740ahj71qvfhxnl304kxzkp4cy1g00ilk8"))

(define rust-gst-plugin-version-helper-0.8.3
  (crate-source "gst-plugin-version-helper" "0.8.3"
                "06djskcdh6ykrkdk4kcdjvyn6aq1v7svw78fjm5haf6py978k2m6"))

(define rust-gstreamer-0.24.1
  (crate-source "gstreamer" "0.24.1"
                "1ii7jiwdjdgi9acvnxngry0vp55qm250b1alsc5ggk6m998xpx9j"))

(define rust-gstreamer-allocators-0.24.0
  (crate-source "gstreamer-allocators" "0.24.0"
                "0azp9rsf894mwbljhzv9v49h224ymkvdr8zb3699j1mnxl354s3w"))

(define rust-gstreamer-allocators-sys-0.24.0
  ;; TODO: Check bundled sources.
  (crate-source "gstreamer-allocators-sys" "0.24.0"
                "01iizg7da489c9m3w2rfl3hck3i4wpadk0d429nsqlbrlaiqxzp5"))

(define rust-gstreamer-base-0.24.0
  (crate-source "gstreamer-base" "0.24.0"
                "0m27bsj1qcahhljm8mrs3ws0ng876x732aj0gvay10504qvmsx1l"))

(define rust-gstreamer-base-sys-0.24.0
  ;; TODO: Check bundled sources.
  (crate-source "gstreamer-base-sys" "0.24.0"
                "14gap610vgm9s3gmm4lxy2962qx3jm89vvzy0qii7n6xcgx01bfz"))

(define rust-gstreamer-gl-0.24.1
  (crate-source "gstreamer-gl" "0.24.1"
                "1cqjv2ywiwm0mn6d7a9s1498i1yw0p6gs6wxzdrvkwqm4d8k11mm"))

(define rust-gstreamer-gl-egl-0.24.0
  (crate-source "gstreamer-gl-egl" "0.24.0"
                "16ly7kx2rzd3y8540vbx4hff368vpn749p0j73wslklj5nw8pypy"))

(define rust-gstreamer-gl-egl-sys-0.24.0
  ;; TODO: Check bundled sources.
  (crate-source "gstreamer-gl-egl-sys" "0.24.0"
                "0j8lgax2w4i316cw47k8cd8kks26p4xj939hhh9hrfbp127l8b6b"))

(define rust-gstreamer-gl-sys-0.24.1
  ;; TODO: Check bundled sources.
  (crate-source "gstreamer-gl-sys" "0.24.1"
                "0g7jwrbqmlq3slqaz6hhh6868hj489dic3jvcjjc360rqpl7pbdv"))

(define rust-gstreamer-gl-wayland-0.24.0
  (crate-source "gstreamer-gl-wayland" "0.24.0"
                "18h4581d0xz90cl9rhzm2wwh088wn8zfncn0ch2b33swpgj3r16d"))

(define rust-gstreamer-gl-wayland-sys-0.24.0
  ;; TODO: Check bundled sources.
  (crate-source "gstreamer-gl-wayland-sys" "0.24.0"
                "13i0kl99a68ihhfxnhlnass1z5d5hsp47j6nmjip96cwzack5wjc"))

(define rust-gstreamer-gl-x11-0.24.0
  (crate-source "gstreamer-gl-x11" "0.24.0"
                "07z2awvndksj652g4l5d3ljxs7vmaw11dy25d4j2kdaghr2cbwxb"))

(define rust-gstreamer-gl-x11-sys-0.24.0
  ;; TODO: Check bundled sources.
  (crate-source "gstreamer-gl-x11-sys" "0.24.0"
                "1pj043hgr3c85psn3v7dhdnkkjchy0spqccp2srff71kalkchg96"))

(define rust-gstreamer-sys-0.24.0
  ;; TODO: Check bundled sources.
  (crate-source "gstreamer-sys" "0.24.0"
                "1yawrr45ymgdhjdaja7j28qmqddmic8x469zr86mljywz4snpx1n"))

(define rust-gstreamer-video-0.24.1
  (crate-source "gstreamer-video" "0.24.1"
                "0lhnsgvlhldrxrzgcs4w4cpdvrq4rabl5szaaai3ymmmqvdy9852"))

(define rust-gstreamer-video-sys-0.24.1
  ;; TODO: Check bundled sources.
  (crate-source "gstreamer-video-sys" "0.24.1"
                "19rpp1pv1415fl1bvlr3hr5ijaaawfk5v6mf08magmxxj8a4p54d"))

(define rust-gtk4-0.10.0
  (crate-source "gtk4" "0.10.0"
                "0gjv60kdg4anhn9zamkqjkjjw0mwq5kx8c0cf7pda2h88fnni3ck"))

(define rust-gtk4-macros-0.10.0
  (crate-source "gtk4-macros" "0.10.0"
                "0p1inz7cpm6xxv8hkiaq9gsy9ch8q7nncn9cp4130qwmhq3d44h9"))

(define rust-gtk4-sys-0.10.0
  ;; TODO: Check bundled sources.
  (crate-source "gtk4-sys" "0.10.0"
                "0x0wdb1frm3d1cnql5z021l8mqxw5i1j9pk226026rz4037vs8x9"))

(define rust-if-chain-1.0.2
  (crate-source "if_chain" "1.0.2"
                "1vdrylnqw8vd99j20jgalh1vp1vh7dwnkdzsmlx4yjsvfsmf2mnb"))

(define rust-indexmap-2.11.0
  (crate-source "indexmap" "2.11.0"
                "1sb3nmhisf9pdwfcxzqlvx97xifcvlh5g0rqj9j7i7qg8f01jj7j"))

(define rust-libadwaita-0.8.0
  (crate-source "libadwaita" "0.8.0"
                "0hbwbdfyv742qpz080xi8csxfafidqkpmdwm684qrgap29fp3xjd"))

(define rust-libadwaita-sys-0.8.0
  ;; TODO: Check bundled sources.
  (crate-source "libadwaita-sys" "0.8.0"
                "1qksqd6a25rsmn7g5aakgbdfx3vl76d8bzxirm80466cj009by7x"))

(define rust-muldiv-1.0.1
  (crate-source "muldiv" "1.0.1"
                "1c6ljsp41n8ijsx7zicwfm135drgyhcms12668ivvsbm1r98frwm"))

(define rust-option-operations-0.5.0
  (crate-source "option-operations" "0.5.0"
                "1l13n9487gk6063zzjdwzqbig78n8mh6dxsbiq9nbaxfn5xx49kw"))

(define rust-pango-0.21.1
  (crate-source "pango" "0.21.1"
                "1zk2wffx29qs1dgx8z9sfca8i7qmp72j11mnmvnn999s82rzwixb"))

(define rust-pango-sys-0.21.1
  ;; TODO: Check bundled sources.
  (crate-source "pango-sys" "0.21.1"
                "11kc250wklyraz98p6syzqq4qlc13xp0hydfkwa4wr27nk65p18z"))

(define rust-proc-macro-crate-3.3.0
  (crate-source "proc-macro-crate" "3.3.0"
                "0d9xlymplfi9yv3f5g4bp0d6qh70apnihvqcjllampx4f5lmikpd"))

(define rust-system-deps-7.0.5
  (crate-source "system-deps" "7.0.5"
                "1nzzhqm3nnzacpcs5q1rlbzxrb1hq6xl5g8mqqnnv2ds1jm57gp4"))

(define rust-target-lexicon-0.13.2
  (crate-source "target-lexicon" "0.13.2"
                "16m6smfz533im9dyxfhnzmpi4af75g2iii36ylc4gfmqvf6gf0p5"))

(define rust-temp-dir-0.1.16
  (crate-source "temp-dir" "0.1.16"
                "0r09qwiiqm8pk6inaqmmp0h6zjg9py6m1dkcwqgghv21x5cnf5w3"))

(define rust-thiserror-2.0.16
  (crate-source "thiserror" "2.0.16"
                "1h30bqyjn5s9ypm668yd9849371rzwk185klwgjg503k2hadcrrl"))

(define rust-thiserror-impl-2.0.16
  (crate-source "thiserror-impl" "2.0.16"
                "0q3r1ipr1rhff6cgrcvc0njffw17rpcqz9hdc7p754cbqkhinpkc"))

(define rust-toml-edit-0.23.4
  (crate-source "toml_edit" "0.23.4"
                "14yfvnljknmh8bp0n2szl8fbxslnwgzsjz9vcvhxlfhdiwdzy4bj"))

(define rust-winnow-0.5.40
  (crate-source "winnow" "0.5.40"
                "0xk8maai7gyxda673mmw3pj1hdizy5fpi7287vaywykkk19sk4zm"))

(define-cargo-inputs lookup-cargo-inputs
                     (binsider =>
                               (list rust-addr2line-0.25.1
                                     rust-adler2-2.0.1
                                     rust-aho-corasick-1.1.4
                                     rust-allocator-api2-0.2.21
                                     rust-android-system-properties-0.1.5
                                     rust-ansi-to-tui-8.0.1
                                     rust-anstream-0.6.21
                                     rust-anstyle-1.0.13
                                     rust-anstyle-parse-0.2.7
                                     rust-anstyle-query-1.1.5
                                     rust-anstyle-wincon-3.0.11
                                     rust-anyhow-1.0.100
                                     rust-arboard-3.6.1
                                     rust-atomic-0.6.1
                                     rust-autocfg-1.5.0
                                     rust-backtrace-0.3.76
                                     rust-base64-0.22.1
                                     rust-better-panic-0.3.0
                                     rust-bit-set-0.5.3
                                     rust-bit-vec-0.6.3
                                     rust-bitflags-1.3.2
                                     rust-bitflags-2.10.0
                                     rust-block-buffer-0.10.4
                                     rust-bumpalo-3.19.1
                                     rust-bytemuck-1.24.0
                                     rust-byteorder-1.5.0
                                     rust-bytes-1.11.0
                                     rust-bytesize-2.3.1
                                     rust-castaway-0.2.4
                                     rust-cc-1.2.52
                                     rust-cesu8-1.1.0
                                     rust-cfg-if-1.0.4
                                     rust-cfg-aliases-0.2.1
                                     rust-chrono-0.4.43
                                     rust-clap-4.5.54
                                     rust-clap-builder-4.5.54
                                     rust-clap-derive-4.5.49
                                     rust-clap-lex-0.7.7
                                     rust-clipboard-win-5.4.1
                                     rust-colorchoice-1.0.4
                                     rust-combine-4.6.7
                                     rust-comfy-table-7.2.2
                                     rust-compact-str-0.9.0
                                     rust-console-0.15.11
                                     rust-console-0.16.2
                                     rust-convert-case-0.10.0
                                     rust-core-foundation-0.10.1
                                     rust-core-foundation-sys-0.8.7
                                     rust-cpufeatures-0.2.17
                                     rust-crossbeam-0.8.4
                                     rust-crossbeam-channel-0.5.15
                                     rust-crossbeam-deque-0.8.6
                                     rust-crossbeam-epoch-0.9.18
                                     rust-crossbeam-queue-0.3.12
                                     rust-crossbeam-utils-0.8.21
                                     rust-crossterm-0.28.1
                                     rust-crossterm-0.29.0
                                     rust-crossterm-winapi-0.9.1
                                     rust-crypto-common-0.1.7
                                     rust-csscolorparser-0.6.2
                                     rust-darling-0.20.11
                                     rust-darling-0.23.0
                                     rust-darling-core-0.20.11
                                     rust-darling-core-0.23.0
                                     rust-darling-macro-0.20.11
                                     rust-darling-macro-0.23.0
                                     rust-deltae-0.3.2
                                     rust-deranged-0.5.5
                                     rust-derive-getters-0.5.0
                                     rust-derive-builder-0.20.2
                                     rust-derive-builder-core-0.20.2
                                     rust-derive-builder-macro-0.20.2
                                     rust-derive-more-2.1.1
                                     rust-derive-more-impl-2.1.1
                                     rust-derive-setters-0.1.8
                                     rust-diff-0.1.13
                                     rust-digest-0.10.7
                                     rust-dispatch2-0.3.0
                                     rust-displaydoc-0.2.5
                                     rust-document-features-0.2.12
                                     rust-downcast-0.11.0
                                     rust-either-1.15.0
                                     rust-elf-0.8.0
                                     rust-encode-unicode-1.0.0
                                     rust-env-home-0.1.0
                                     rust-equivalent-1.0.2
                                     rust-errno-0.3.14
                                     rust-error-code-3.3.2
                                     rust-euclid-0.22.11
                                     rust-fancy-regex-0.11.0
                                     rust-filedescriptor-0.8.3
                                     rust-find-msvc-tools-0.1.7
                                     rust-finl-unicode-1.4.0
                                     rust-fixedbitset-0.4.2
                                     rust-fnv-1.0.7
                                     rust-foldhash-0.2.0
                                     rust-font8x8-0.3.1
                                     rust-form-urlencoded-1.2.2
                                     rust-fragile-2.0.1
                                     rust-fs-err-3.2.2
                                     rust-generic-array-0.14.7
                                     rust-gethostname-1.1.0
                                     rust-getrandom-0.3.4
                                     rust-gimli-0.32.3
                                     rust-glob-0.3.3
                                     rust-goblin-0.10.4
                                     rust-hashbrown-0.16.1
                                     rust-heck-0.5.0
                                     rust-heh-0.6.3
                                     rust-hex-0.4.3
                                     rust-iana-time-zone-0.1.64
                                     rust-iana-time-zone-haiku-0.1.2
                                     rust-icu-collections-2.1.1
                                     rust-icu-locale-core-2.1.1
                                     rust-icu-normalizer-2.1.1
                                     rust-icu-normalizer-data-2.1.1
                                     rust-icu-properties-2.1.2
                                     rust-icu-properties-data-2.1.2
                                     rust-icu-provider-2.1.1
                                     rust-ident-case-1.0.1
                                     rust-idna-1.1.0
                                     rust-idna-adapter-1.2.1
                                     rust-indoc-2.0.7
                                     rust-instability-0.3.11
                                     rust-is-terminal-polyfill-1.70.2
                                     rust-itertools-0.14.0
                                     rust-itoa-1.0.17
                                     rust-jni-0.21.1
                                     rust-jni-sys-0.3.0
                                     rust-js-sys-0.3.85
                                     rust-kasuari-0.4.11
                                     rust-lab-0.11.0
                                     rust-lazy-static-1.5.0
                                     rust-lddtree-0.4.0
                                     rust-libc-0.2.180
                                     rust-line-clipping-0.3.5
                                     rust-linux-raw-sys-0.4.15
                                     rust-linux-raw-sys-0.11.0
                                     rust-litemap-0.8.1
                                     rust-litrs-1.0.0
                                     rust-lock-api-0.4.14
                                     rust-log-0.4.29
                                     rust-lru-0.16.3
                                     rust-lurk-cli-0.3.11
                                     rust-mac-address-1.1.8
                                     rust-memchr-2.7.6
                                     rust-memmap2-0.9.9
                                     rust-memmem-0.1.1
                                     rust-memoffset-0.9.1
                                     rust-minimal-lexical-0.2.1
                                     rust-miniz-oxide-0.8.9
                                     rust-mio-1.1.1
                                     rust-mockall-0.13.1
                                     rust-mockall-derive-0.13.1
                                     rust-ndk-context-0.1.1
                                     rust-nix-0.29.0
                                     rust-nix-0.30.1
                                     rust-nom-7.1.3
                                     rust-nom-8.0.0
                                     rust-num-conv-0.1.0
                                     rust-num-derive-0.4.2
                                     rust-num-traits-0.2.19
                                     rust-num-threads-0.1.7
                                     rust-objc2-0.6.3
                                     rust-objc2-app-kit-0.3.2
                                     rust-objc2-core-foundation-0.3.2
                                     rust-objc2-core-graphics-0.3.2
                                     rust-objc2-encode-4.1.0
                                     rust-objc2-foundation-0.3.2
                                     rust-objc2-io-surface-0.3.2
                                     rust-object-0.37.3
                                     rust-once-cell-1.21.3
                                     rust-once-cell-polyfill-1.70.2
                                     rust-ordered-float-4.6.0
                                     rust-parking-lot-0.12.5
                                     rust-parking-lot-core-0.9.12
                                     rust-percent-encoding-2.3.2
                                     rust-pest-2.8.5
                                     rust-pest-derive-2.8.5
                                     rust-pest-generator-2.8.5
                                     rust-pest-meta-2.8.5
                                     rust-phf-0.11.3
                                     rust-phf-codegen-0.11.3
                                     rust-phf-generator-0.11.3
                                     rust-phf-macros-0.11.3
                                     rust-phf-shared-0.11.3
                                     rust-plain-0.2.3
                                     rust-portable-atomic-1.13.0
                                     rust-potential-utf-0.1.4
                                     rust-powerfmt-0.2.0
                                     rust-predicates-3.1.3
                                     rust-predicates-core-1.0.9
                                     rust-predicates-tree-1.0.12
                                     rust-pretty-assertions-1.4.1
                                     rust-proc-macro2-1.0.105
                                     rust-quote-1.0.43
                                     rust-r-efi-5.3.0
                                     rust-rand-0.8.5
                                     rust-rand-core-0.6.4
                                     rust-ratatui-0.30.0
                                     rust-ratatui-core-0.1.0
                                     rust-ratatui-crossterm-0.1.0
                                     rust-ratatui-macros-0.7.0
                                     rust-ratatui-termwiz-0.1.0
                                     rust-ratatui-widgets-0.3.0
                                     rust-redox-syscall-0.5.18
                                     rust-regex-1.12.2
                                     rust-regex-automata-0.4.13
                                     rust-regex-syntax-0.8.8
                                     rust-rust-strings-0.6.0
                                     rust-rustc-demangle-0.1.27
                                     rust-rustc-version-0.4.1
                                     rust-rustix-0.38.44
                                     rust-rustix-1.1.3
                                     rust-rustversion-1.0.22
                                     rust-ryu-1.0.22
                                     rust-same-file-1.0.6
                                     rust-scopeguard-1.2.0
                                     rust-scroll-0.13.0
                                     rust-scroll-derive-0.13.1
                                     rust-semver-1.0.27
                                     rust-serde-1.0.228
                                     rust-serde-core-1.0.228
                                     rust-serde-derive-1.0.228
                                     rust-serde-json-1.0.149
                                     rust-serde-repr-0.1.20
                                     rust-sha2-0.10.9
                                     rust-shlex-1.3.0
                                     rust-signal-hook-0.3.18
                                     rust-signal-hook-mio-0.2.5
                                     rust-signal-hook-registry-1.4.8
                                     rust-simdutf8-0.1.5
                                     rust-simplelog-0.12.2
                                     rust-siphasher-1.0.1
                                     rust-smallvec-1.15.1
                                     rust-smawk-0.3.2
                                     rust-stable-deref-trait-1.2.1
                                     rust-static-assertions-1.1.0
                                     rust-strsim-0.11.1
                                     rust-strum-0.27.2
                                     rust-strum-macros-0.27.2
                                     rust-syn-1.0.109
                                     rust-syn-2.0.114
                                     rust-synstructure-0.13.2
                                     rust-syscalls-0.7.0
                                     rust-sysinfo-0.37.2
                                     rust-termbg-0.6.2
                                     rust-termcolor-1.4.1
                                     rust-terminal-size-0.4.3
                                     rust-terminfo-0.9.0
                                     rust-termios-0.3.3
                                     rust-termtree-0.5.1
                                     rust-termwiz-0.23.3
                                     rust-textwrap-0.16.2
                                     rust-thiserror-1.0.69
                                     rust-thiserror-2.0.17
                                     rust-thiserror-impl-1.0.69
                                     rust-thiserror-impl-2.0.17
                                     rust-time-0.3.45
                                     rust-time-core-0.1.7
                                     rust-time-macros-0.2.25
                                     rust-tinystr-0.8.2
                                     rust-tui-big-text-0.8.1
                                     rust-tui-input-0.15.0
                                     rust-tui-popup-0.7.2
                                     rust-typenum-1.19.0
                                     rust-ucd-trie-0.1.7
                                     rust-unicode-ident-1.0.22
                                     rust-unicode-linebreak-0.1.5
                                     rust-unicode-segmentation-1.12.0
                                     rust-unicode-truncate-2.0.1
                                     rust-unicode-width-0.2.2
                                     rust-url-2.5.8
                                     rust-utf8-iter-1.0.4
                                     rust-utf8parse-0.2.2
                                     rust-uuid-1.19.0
                                     rust-uzers-0.12.2
                                     rust-version-check-0.9.5
                                     rust-vtparse-0.6.2
                                     rust-walkdir-2.5.0
                                     rust-wasi-0.11.1+wasi-snapshot-preview1
                                     rust-wasip2-1.0.2+wasi-0.2.9
                                     rust-wasm-bindgen-0.2.108
                                     rust-wasm-bindgen-macro-0.2.108
                                     rust-wasm-bindgen-macro-support-0.2.108
                                     rust-wasm-bindgen-shared-0.2.108
                                     rust-web-sys-0.3.85
                                     rust-webbrowser-1.0.6
                                     rust-wezterm-bidi-0.2.3
                                     rust-wezterm-blob-leases-0.1.1
                                     rust-wezterm-color-types-0.3.0
                                     rust-wezterm-dynamic-0.2.1
                                     rust-wezterm-dynamic-derive-0.1.1
                                     rust-wezterm-input-types-0.1.0
                                     rust-which-8.0.0
                                     rust-winapi-0.3.9
                                     rust-winapi-i686-pc-windows-gnu-0.4.0
                                     rust-winapi-util-0.1.11
                                     rust-winapi-x86-64-pc-windows-gnu-0.4.0
                                     rust-windows-0.61.3
                                     rust-windows-collections-0.2.0
                                     rust-windows-core-0.61.2
                                     rust-windows-core-0.62.2
                                     rust-windows-future-0.2.1
                                     rust-windows-implement-0.60.2
                                     rust-windows-interface-0.59.3
                                     rust-windows-link-0.1.3
                                     rust-windows-link-0.2.1
                                     rust-windows-numerics-0.2.0
                                     rust-windows-result-0.3.4
                                     rust-windows-result-0.4.1
                                     rust-windows-strings-0.4.2
                                     rust-windows-strings-0.5.1
                                     rust-windows-sys-0.45.0
                                     rust-windows-sys-0.59.0
                                     rust-windows-sys-0.60.2
                                     rust-windows-sys-0.61.2
                                     rust-windows-targets-0.42.2
                                     rust-windows-targets-0.52.6
                                     rust-windows-targets-0.53.5
                                     rust-windows-threading-0.1.0
                                     rust-windows-aarch64-gnullvm-0.42.2
                                     rust-windows-aarch64-gnullvm-0.52.6
                                     rust-windows-aarch64-gnullvm-0.53.1
                                     rust-windows-aarch64-msvc-0.42.2
                                     rust-windows-aarch64-msvc-0.52.6
                                     rust-windows-aarch64-msvc-0.53.1
                                     rust-windows-i686-gnu-0.42.2
                                     rust-windows-i686-gnu-0.52.6
                                     rust-windows-i686-gnu-0.53.1
                                     rust-windows-i686-gnullvm-0.52.6
                                     rust-windows-i686-gnullvm-0.53.1
                                     rust-windows-i686-msvc-0.42.2
                                     rust-windows-i686-msvc-0.52.6
                                     rust-windows-i686-msvc-0.53.1
                                     rust-windows-x86-64-gnu-0.42.2
                                     rust-windows-x86-64-gnu-0.52.6
                                     rust-windows-x86-64-gnu-0.53.1
                                     rust-windows-x86-64-gnullvm-0.42.2
                                     rust-windows-x86-64-gnullvm-0.52.6
                                     rust-windows-x86-64-gnullvm-0.53.1
                                     rust-windows-x86-64-msvc-0.42.2
                                     rust-windows-x86-64-msvc-0.52.6
                                     rust-windows-x86-64-msvc-0.53.1
                                     rust-winsafe-0.0.19
                                     rust-wit-bindgen-0.51.0
                                     rust-writeable-0.6.2
                                     rust-x11rb-0.13.2
                                     rust-x11rb-protocol-0.13.2
                                     rust-yansi-1.0.1
                                     rust-yoke-0.8.1
                                     rust-yoke-derive-0.8.1
                                     rust-zerofrom-0.1.6
                                     rust-zerofrom-derive-0.1.6
                                     rust-zerotrie-0.2.3
                                     rust-zerovec-0.11.5
                                     rust-zerovec-derive-0.11.2
                                     rust-zmij-1.0.14))
                     (broot =>
                            (list rust-adler2-2.0.1
                                  rust-ahash-0.8.12
                                  rust-aho-corasick-1.1.4
                                  rust-aligned-vec-0.6.4
                                  rust-allocator-api2-0.2.21
                                  rust-android-system-properties-0.1.5
                                  rust-ansi-colours-1.2.3
                                  rust-anstream-0.6.21
                                  rust-anstyle-1.0.13
                                  rust-anstyle-parse-0.2.7
                                  rust-anstyle-query-1.1.4
                                  rust-anstyle-wincon-3.0.10
                                  rust-anyhow-1.0.100
                                  rust-arbitrary-1.4.2
                                  rust-arg-enum-proc-macro-0.3.4
                                  rust-argh-0.1.13
                                  rust-argh-derive-0.1.13
                                  rust-argh-shared-0.1.13
                                  rust-arrayref-0.3.9
                                  rust-arrayvec-0.7.6
                                  rust-autocfg-1.5.0
                                  rust-av1-grain-0.2.5
                                  rust-avif-serialize-0.8.6
                                  rust-base64-0.13.1
                                  rust-base64-0.22.1
                                  rust-bet-1.1.0
                                  rust-bincode-1.3.3
                                  rust-bit-set-0.5.3
                                  rust-bit-vec-0.6.3
                                  rust-bit-field-0.10.3
                                  rust-bitflags-1.3.2
                                  rust-bitflags-2.10.0
                                  rust-bitstream-io-2.6.0
                                  rust-block2-0.5.1
                                  rust-bstr-1.12.1
                                  rust-built-0.7.7
                                  rust-bumpalo-3.19.0
                                  rust-bytemuck-1.24.0
                                  rust-byteorder-lite-0.1.0
                                  rust-cc-1.2.44
                                  rust-cfg-expr-0.15.8
                                  rust-cfg-if-1.0.4
                                  rust-cfg-aliases-0.2.1
                                  rust-char-reader-0.1.1
                                  rust-chrono-0.4.42
                                  rust-clap-4.5.51
                                  rust-clap-help-1.5.0
                                  rust-clap-builder-4.5.51
                                  rust-clap-complete-4.5.60
                                  rust-clap-derive-4.5.49
                                  rust-clap-lex-0.7.6
                                  rust-clap-mangen-0.2.31
                                  rust-cli-log-2.1.0
                                  rust-clipboard-win-4.5.0
                                  rust-clipboard-macos-0.1.1
                                  rust-color-quant-1.1.0
                                  rust-colorchoice-1.0.4
                                  rust-convert-case-0.7.1
                                  rust-coolor-1.1.0
                                  rust-core-foundation-0.9.4
                                  rust-core-foundation-sys-0.8.7
                                  rust-core-maths-0.1.1
                                  rust-crc32fast-1.5.0
                                  rust-crokey-1.3.0
                                  rust-crokey-proc-macros-1.3.0
                                  rust-crossbeam-0.8.4
                                  rust-crossbeam-channel-0.5.15
                                  rust-crossbeam-deque-0.8.6
                                  rust-crossbeam-epoch-0.9.18
                                  rust-crossbeam-queue-0.3.12
                                  rust-crossbeam-utils-0.8.21
                                  rust-crossterm-0.28.1
                                  rust-crossterm-0.29.0
                                  rust-crossterm-winapi-0.9.1
                                  rust-crunchy-0.2.4
                                  rust-csv-1.4.0
                                  rust-csv-core-0.1.13
                                  rust-csv2svg-0.2.3
                                  rust-custom-error-1.9.2
                                  rust-data-url-0.3.2
                                  rust-deranged-0.5.5
                                  rust-derive-more-2.0.1
                                  rust-derive-more-impl-2.0.1
                                  rust-deser-hjson-2.2.4
                                  rust-directories-4.0.1
                                  rust-directories-5.0.1
                                  rust-dirs-sys-0.3.7
                                  rust-dirs-sys-0.4.1
                                  rust-displaydoc-0.2.5
                                  rust-doc-comment-0.3.4
                                  rust-document-features-0.2.12
                                  rust-either-1.15.0
                                  rust-equator-0.4.2
                                  rust-equator-macro-0.4.2
                                  rust-equivalent-1.0.2
                                  rust-errno-0.3.14
                                  rust-error-code-2.3.1
                                  rust-euclid-0.22.11
                                  rust-exr-1.73.0
                                  rust-fallible-iterator-0.3.0
                                  rust-fallible-streaming-iterator-0.1.9
                                  rust-fancy-regex-0.11.0
                                  rust-fastrand-2.3.0
                                  rust-fax-0.2.6
                                  rust-fax-derive-0.2.0
                                  rust-fdeflate-0.3.7
                                  rust-file-size-1.0.3
                                  rust-find-msvc-tools-0.1.4
                                  rust-flate2-1.1.5
                                  rust-flex-grow-0.1.0
                                  rust-float-cmp-0.9.0
                                  rust-fnv-1.0.7
                                  rust-foldhash-0.2.0
                                  rust-fontconfig-parser-0.5.8
                                  rust-fontdb-0.23.0
                                  rust-form-urlencoded-1.2.2
                                  rust-fsevent-sys-4.1.0
                                  rust-gethostname-0.3.0
                                  rust-getrandom-0.2.16
                                  rust-getrandom-0.3.4
                                  rust-gif-0.13.3
                                  rust-git2-0.20.2
                                  rust-glassbench-0.4.4
                                  rust-glob-0.3.3
                                  rust-half-2.7.1
                                  rust-hashbrown-0.14.5
                                  rust-hashbrown-0.16.0
                                  rust-hashlink-0.9.1
                                  rust-heck-0.4.1
                                  rust-heck-0.5.0
                                  rust-iana-time-zone-0.1.64
                                  rust-iana-time-zone-haiku-0.1.2
                                  rust-icu-collections-2.1.1
                                  rust-icu-locale-core-2.1.1
                                  rust-icu-normalizer-2.1.1
                                  rust-icu-normalizer-data-2.1.1
                                  rust-icu-properties-2.1.1
                                  rust-icu-properties-data-2.1.1
                                  rust-icu-provider-2.1.1
                                  rust-id-arena-2.2.1
                                  rust-idna-1.1.0
                                  rust-idna-adapter-1.2.1
                                  rust-image-0.25.8
                                  rust-image-webp-0.2.4
                                  rust-imagesize-0.13.0
                                  rust-imgref-1.12.0
                                  rust-include-dir-0.7.4
                                  rust-include-dir-macros-0.7.4
                                  rust-indexmap-2.12.0
                                  rust-inotify-0.11.0
                                  rust-inotify-sys-0.1.5
                                  rust-interpolate-name-0.2.4
                                  rust-io-kit-sys-0.4.1
                                  rust-is-executable-1.0.5
                                  rust-is-terminal-polyfill-1.70.2
                                  rust-itertools-0.12.1
                                  rust-itoa-1.0.15
                                  rust-jobserver-0.1.34
                                  rust-jpeg-encoder-0.5.1
                                  rust-js-sys-0.3.82
                                  rust-jxl-bitstream-0.2.3
                                  rust-jxl-coding-0.2.3
                                  rust-jxl-color-0.3.2
                                  rust-jxl-frame-0.5.0
                                  rust-jxl-grid-0.1.1
                                  rust-jxl-image-0.5.0
                                  rust-jxl-modular-0.3.0
                                  rust-jxl-oxide-0.4.0
                                  rust-jxl-render-0.4.0
                                  rust-jxl-vardct-0.3.0
                                  rust-kamadak-exif-0.5.5
                                  rust-kqueue-1.1.1
                                  rust-kqueue-sys-1.0.4
                                  rust-kurbo-0.11.3
                                  rust-lazy-regex-3.4.2
                                  rust-lazy-regex-proc-macros-3.4.2
                                  rust-lazy-static-1.5.0
                                  rust-lebe-0.5.3
                                  rust-lfs-core-0.17.0
                                  rust-libc-0.2.177
                                  rust-libfuzzer-sys-0.4.10
                                  rust-libgit2-sys-0.18.2+1.9.1
                                  rust-libm-0.2.15
                                  rust-libredox-0.1.10
                                  rust-libsqlite3-sys-0.30.1
                                  rust-libz-sys-1.1.22
                                  rust-linked-hash-map-0.5.6
                                  rust-linux-raw-sys-0.4.15
                                  rust-linux-raw-sys-0.11.0
                                  rust-litemap-0.8.1
                                  rust-litrs-1.0.0
                                  rust-lock-api-0.4.14
                                  rust-log-0.4.28
                                  rust-loop9-0.1.5
                                  rust-lru-0.16.2
                                  rust-mach2-0.4.3
                                  rust-maybe-rayon-0.1.1
                                  rust-memchr-2.7.6
                                  rust-memmap2-0.9.9
                                  rust-memoffset-0.7.1
                                  rust-minimad-0.13.1
                                  rust-miniz-oxide-0.8.9
                                  rust-mio-1.1.0
                                  rust-moxcms-0.7.9
                                  rust-mutate-once-0.1.2
                                  rust-new-debug-unreachable-1.0.6
                                  rust-nix-0.26.4
                                  rust-nix-0.29.0
                                  rust-nom-8.0.0
                                  rust-noop-proc-macro-0.3.0
                                  rust-normpath-1.5.0
                                  rust-notify-8.2.0
                                  rust-notify-types-2.0.0
                                  rust-num-bigint-0.4.6
                                  rust-num-conv-0.1.0
                                  rust-num-derive-0.4.2
                                  rust-num-integer-0.1.46
                                  rust-num-rational-0.4.2
                                  rust-num-traits-0.2.19
                                  rust-objc-sys-0.3.5
                                  rust-objc2-0.5.2
                                  rust-objc2-0.6.3
                                  rust-objc2-app-kit-0.2.2
                                  rust-objc2-core-data-0.2.2
                                  rust-objc2-core-image-0.2.2
                                  rust-objc2-encode-4.1.0
                                  rust-objc2-foundation-0.2.2
                                  rust-objc2-foundation-0.3.2
                                  rust-objc2-metal-0.2.2
                                  rust-objc2-quartz-core-0.2.2
                                  rust-once-cell-1.21.3
                                  rust-once-cell-polyfill-1.70.2
                                  rust-open-1.7.1
                                  rust-opener-0.8.3
                                  rust-option-ext-0.2.0
                                  rust-parking-lot-0.12.5
                                  rust-parking-lot-core-0.9.12
                                  rust-paste-1.0.15
                                  rust-pathdiff-0.2.3
                                  rust-percent-encoding-2.3.2
                                  rust-phf-0.13.1
                                  rust-phf-generator-0.13.1
                                  rust-phf-macros-0.13.1
                                  rust-phf-shared-0.13.1
                                  rust-pico-args-0.5.0
                                  rust-pin-project-lite-0.2.16
                                  rust-pkg-config-0.3.32
                                  rust-plist-1.8.0
                                  rust-png-0.17.16
                                  rust-png-0.18.0
                                  rust-potential-utf-0.1.4
                                  rust-powerfmt-0.2.0
                                  rust-ppv-lite86-0.2.21
                                  rust-proc-macro2-1.0.103
                                  rust-proc-status-0.1.1
                                  rust-profiling-1.0.17
                                  rust-profiling-procmacros-1.0.17
                                  rust-pxfm-0.1.25
                                  rust-qoi-0.4.1
                                  rust-quick-error-2.0.1
                                  rust-quick-xml-0.38.3
                                  rust-quote-1.0.41
                                  rust-r-efi-5.3.0
                                  rust-rand-0.8.5
                                  rust-rand-chacha-0.3.1
                                  rust-rand-core-0.6.4
                                  rust-rav1e-0.7.1
                                  rust-ravif-0.11.20
                                  rust-rayon-1.11.0
                                  rust-rayon-core-1.13.0
                                  rust-redox-syscall-0.5.18
                                  rust-redox-users-0.4.6
                                  rust-regex-1.12.2
                                  rust-regex-automata-0.4.13
                                  rust-regex-syntax-0.8.8
                                  rust-resvg-0.45.1
                                  rust-rgb-0.8.52
                                  rust-roff-0.2.2
                                  rust-roxmltree-0.20.0
                                  rust-rusqlite-0.32.1
                                  rust-rust-fuzzy-search-0.1.1
                                  rust-rustc-hash-2.1.1
                                  rust-rustix-0.38.44
                                  rust-rustix-1.1.2
                                  rust-rustversion-1.0.22
                                  rust-rustybuzz-0.20.1
                                  rust-ryu-1.0.20
                                  rust-same-file-1.0.6
                                  rust-scopeguard-1.2.0
                                  rust-secular-1.0.1
                                  rust-serde-1.0.228
                                  rust-serde-core-1.0.228
                                  rust-serde-derive-1.0.228
                                  rust-serde-json-1.0.145
                                  rust-serde-spanned-0.6.9
                                  rust-serde-spanned-1.0.3
                                  rust-shlex-1.3.0
                                  rust-signal-hook-0.3.18
                                  rust-signal-hook-mio-0.2.5
                                  rust-signal-hook-registry-1.4.6
                                  rust-simd-adler32-0.3.7
                                  rust-simd-helpers-0.1.0
                                  rust-simplecss-0.2.2
                                  rust-siphasher-1.0.1
                                  rust-slotmap-1.0.7
                                  rust-smallvec-1.15.1
                                  rust-snafu-0.7.5
                                  rust-snafu-derive-0.7.5
                                  rust-splitty-1.0.2
                                  rust-stable-deref-trait-1.2.1
                                  rust-str-buf-1.0.6
                                  rust-strict-0.2.0
                                  rust-strict-num-0.1.1
                                  rust-strsim-0.11.1
                                  rust-svg-0.16.0
                                  rust-svgtypes-0.15.3
                                  rust-syn-1.0.109
                                  rust-syn-2.0.108
                                  rust-synstructure-0.13.2
                                  rust-syntect-no-panic-6.0.0
                                  rust-system-deps-6.2.2
                                  rust-target-lexicon-0.12.16
                                  rust-tempfile-3.23.0
                                  rust-termimad-0.31.3
                                  rust-termimad-0.34.0
                                  rust-terminal-clipboard-0.4.1
                                  rust-terminal-light-1.8.0
                                  rust-termux-clipboard-0.1.0
                                  rust-thiserror-1.0.69
                                  rust-thiserror-2.0.17
                                  rust-thiserror-impl-1.0.69
                                  rust-thiserror-impl-2.0.17
                                  rust-tiff-0.10.3
                                  rust-time-0.3.44
                                  rust-time-core-0.1.6
                                  rust-time-macros-0.2.24
                                  rust-tiny-skia-0.11.4
                                  rust-tiny-skia-path-0.11.4
                                  rust-tinystr-0.8.2
                                  rust-tinyvec-1.10.0
                                  rust-tinyvec-macros-0.1.1
                                  rust-toml-0.8.23
                                  rust-toml-0.9.8
                                  rust-toml-datetime-0.6.11
                                  rust-toml-datetime-0.7.3
                                  rust-toml-edit-0.22.27
                                  rust-toml-parser-1.0.4
                                  rust-toml-writer-1.0.4
                                  rust-tracing-0.1.41
                                  rust-tracing-core-0.1.34
                                  rust-trash-5.2.5
                                  rust-ttf-parser-0.25.1
                                  rust-umask-2.1.0
                                  rust-unicode-bidi-0.3.18
                                  rust-unicode-bidi-mirroring-0.4.0
                                  rust-unicode-ccc-0.4.0
                                  rust-unicode-ident-1.0.22
                                  rust-unicode-normalization-0.1.25
                                  rust-unicode-properties-0.1.4
                                  rust-unicode-script-0.5.7
                                  rust-unicode-segmentation-1.12.0
                                  rust-unicode-vo-0.1.0
                                  rust-unicode-width-0.1.14
                                  rust-unicode-width-0.2.2
                                  rust-url-2.5.7
                                  rust-urlencoding-2.1.3
                                  rust-usvg-0.45.1
                                  rust-utf8-iter-1.0.4
                                  rust-utf8parse-0.2.2
                                  rust-uzers-0.12.1
                                  rust-v-frame-0.3.9
                                  rust-vcpkg-0.2.15
                                  rust-version-compare-0.2.1
                                  rust-version-check-0.9.5
                                  rust-vte-0.15.0
                                  rust-walkdir-2.5.0
                                  rust-wasi-0.11.1+wasi-snapshot-preview1
                                  rust-wasip2-1.0.1+wasi-0.2.4
                                  rust-wasm-bindgen-0.2.105
                                  rust-wasm-bindgen-macro-0.2.105
                                  rust-wasm-bindgen-macro-support-0.2.105
                                  rust-wasm-bindgen-shared-0.2.105
                                  rust-weezl-0.1.10
                                  rust-which-4.4.0
                                  rust-winapi-0.3.9
                                  rust-winapi-i686-pc-windows-gnu-0.4.0
                                  rust-winapi-util-0.1.11
                                  rust-winapi-wsapoll-0.1.2
                                  rust-winapi-x86-64-pc-windows-gnu-0.4.0
                                  rust-windows-0.56.0
                                  rust-windows-0.62.2
                                  rust-windows-collections-0.3.2
                                  rust-windows-core-0.56.0
                                  rust-windows-core-0.62.2
                                  rust-windows-future-0.3.2
                                  rust-windows-implement-0.56.0
                                  rust-windows-implement-0.60.2
                                  rust-windows-interface-0.56.0
                                  rust-windows-interface-0.59.3
                                  rust-windows-link-0.2.1
                                  rust-windows-numerics-0.3.1
                                  rust-windows-result-0.1.2
                                  rust-windows-result-0.4.1
                                  rust-windows-strings-0.5.1
                                  rust-windows-sys-0.48.0
                                  rust-windows-sys-0.59.0
                                  rust-windows-sys-0.60.2
                                  rust-windows-sys-0.61.2
                                  rust-windows-targets-0.48.5
                                  rust-windows-targets-0.52.6
                                  rust-windows-targets-0.53.5
                                  rust-windows-threading-0.2.1
                                  rust-windows-aarch64-gnullvm-0.48.5
                                  rust-windows-aarch64-gnullvm-0.52.6
                                  rust-windows-aarch64-gnullvm-0.53.1
                                  rust-windows-aarch64-msvc-0.48.5
                                  rust-windows-aarch64-msvc-0.52.6
                                  rust-windows-aarch64-msvc-0.53.1
                                  rust-windows-i686-gnu-0.48.5
                                  rust-windows-i686-gnu-0.52.6
                                  rust-windows-i686-gnu-0.53.1
                                  rust-windows-i686-gnullvm-0.52.6
                                  rust-windows-i686-gnullvm-0.53.1
                                  rust-windows-i686-msvc-0.48.5
                                  rust-windows-i686-msvc-0.52.6
                                  rust-windows-i686-msvc-0.53.1
                                  rust-windows-x86-64-gnu-0.48.5
                                  rust-windows-x86-64-gnu-0.52.6
                                  rust-windows-x86-64-gnu-0.53.1
                                  rust-windows-x86-64-gnullvm-0.48.5
                                  rust-windows-x86-64-gnullvm-0.52.6
                                  rust-windows-x86-64-gnullvm-0.53.1
                                  rust-windows-x86-64-msvc-0.48.5
                                  rust-windows-x86-64-msvc-0.52.6
                                  rust-windows-x86-64-msvc-0.53.1
                                  rust-winnow-0.7.13
                                  rust-wit-bindgen-0.46.0
                                  rust-writeable-0.6.2
                                  rust-x11-clipboard-0.8.1
                                  rust-x11rb-0.12.0
                                  rust-x11rb-protocol-0.12.0
                                  rust-xmlwriter-0.1.0
                                  rust-xterm-query-0.5.2
                                  rust-yaml-rust-0.4.5
                                  rust-yoke-0.8.1
                                  rust-yoke-derive-0.8.1
                                  rust-zerocopy-0.8.27
                                  rust-zerocopy-derive-0.8.27
                                  rust-zerofrom-0.1.6
                                  rust-zerofrom-derive-0.1.6
                                  rust-zerotrie-0.2.3
                                  rust-zerovec-0.11.5
                                  rust-zerovec-derive-0.11.2
                                  rust-zune-bmp-0.4.0
                                  rust-zune-core-0.4.12
                                  rust-zune-farbfeld-0.4.0
                                  rust-zune-hdr-0.4.0
                                  rust-zune-image-0.4.15
                                  rust-zune-inflate-0.2.54
                                  rust-zune-jpeg-0.4.21
                                  rust-zune-jpegxl-0.4.0
                                  rust-zune-png-0.4.10
                                  rust-zune-ppm-0.4.0
                                  rust-zune-psd-0.4.0
                                  rust-zune-qoi-0.4.10))
                     (himalaya =>
                               (list rust-inflector-0.11.4
                                rust-abnf-core-0.6.0
                                rust-addr2line-0.21.0
                                rust-adler-1.0.2
                                rust-adler2-2.0.0
                                rust-aes-0.8.4
                                rust-ahash-0.8.11
                                rust-aho-corasick-1.1.3
                                rust-aliasable-0.1.3
                                rust-allocator-api2-0.2.21
                                rust-android-tzdata-0.1.1
                                rust-android-system-properties-0.1.5
                                rust-anstream-0.6.18
                                rust-anstyle-1.0.10
                                rust-anstyle-parse-0.2.6
                                rust-anstyle-query-1.1.2
                                rust-anstyle-wincon-3.0.6
                                rust-ariadne-0.2.0
                                rust-async-broadcast-0.7.2
                                rust-async-channel-2.3.1
                                rust-async-executor-1.13.1
                                rust-async-fs-2.1.2
                                rust-async-io-2.4.0
                                rust-async-lock-3.4.0
                                rust-async-process-2.3.0
                                rust-async-recursion-1.1.1
                                rust-async-signal-0.2.10
                                rust-async-task-4.7.1
                                rust-async-trait-0.1.85
                                rust-atomic-waker-1.1.2
                                rust-autocfg-1.4.0
                                rust-backtrace-0.3.71
                                rust-base16ct-0.2.0
                                rust-base64-0.13.1
                                rust-base64-0.21.7
                                rust-base64-0.22.1
                                rust-base64ct-1.6.0
                                rust-bitfield-0.14.0
                                rust-bitflags-1.3.2
                                rust-bitflags-2.7.0
                                rust-block-buffer-0.10.4
                                rust-block-padding-0.3.3
                                rust-blocking-1.6.1
                                rust-blowfish-0.9.1
                                rust-bounded-static-0.8.0
                                rust-bounded-static-derive-0.8.0
                                rust-bstr-1.11.3
                                rust-buffer-redux-1.0.2
                                rust-build-rs-0.1.2
                                rust-bumpalo-3.16.0
                                rust-byteorder-1.5.0
                                rust-bytes-1.9.0
                                rust-camellia-0.1.0
                                rust-cast5-0.11.1
                                rust-cbc-0.1.2
                                rust-cc-1.2.8
                                rust-cesu8-1.1.0
                                rust-cfb-mode-0.8.2
                                rust-cfg-expr-0.15.8
                                rust-cfg-if-1.0.0
                                rust-cfg-aliases-0.2.1
                                rust-chrono-0.4.39
                                rust-chumsky-1.0.0-alpha.7
                                rust-cipher-0.4.4
                                rust-clap-4.5.26
                                rust-clap-builder-4.5.26
                                rust-clap-complete-4.5.42
                                rust-clap-derive-4.5.24
                                rust-clap-lex-0.7.4
                                rust-clap-mangen-0.2.26
                                rust-color-eyre-0.6.3
                                rust-color-spantrace-0.2.1
                                rust-colorchoice-1.0.3
                                rust-combine-4.6.7
                                rust-comfy-table-7.1.3
                                rust-concurrent-queue-2.5.0
                                rust-const-oid-0.9.6
                                rust-conv-0.3.3
                                rust-core-foundation-0.9.4
                                rust-core-foundation-0.10.0
                                rust-core-foundation-sys-0.8.7
                                rust-cpufeatures-0.2.16
                                rust-crc24-0.1.6
                                rust-crc32fast-1.4.2
                                rust-crossbeam-deque-0.8.6
                                rust-crossbeam-epoch-0.9.18
                                rust-crossbeam-utils-0.8.21
                                rust-crossterm-0.25.0
                                rust-crossterm-0.27.0
                                rust-crossterm-0.28.1
                                rust-crossterm-winapi-0.9.1
                                rust-crypto-bigint-0.5.5
                                rust-crypto-common-0.1.6
                                rust-cstr-argument-0.1.2
                                rust-curve25519-dalek-4.1.3
                                rust-curve25519-dalek-derive-0.1.1
                                rust-custom-derive-0.1.7
                                rust-darling-0.10.2
                                rust-darling-0.14.4
                                rust-darling-core-0.10.2
                                rust-darling-core-0.14.4
                                rust-darling-macro-0.10.2
                                rust-darling-macro-0.14.4
                                rust-data-encoding-2.6.0
                                rust-dbus-0.9.7
                                rust-dbus-secret-service-4.0.3
                                rust-der-0.7.9
                                rust-derive-builder-0.12.0
                                rust-derive-builder-core-0.12.0
                                rust-derive-builder-macro-0.12.0
                                rust-des-0.8.1
                                rust-digest-0.10.7
                                rust-dirs-4.0.0
                                rust-dirs-5.0.1
                                rust-dirs-sys-0.3.7
                                rust-dirs-sys-0.4.1
                                rust-displaydoc-0.2.5
                                rust-dyn-clone-1.0.17
                                rust-ecdsa-0.16.9
                                rust-ed25519-2.2.3
                                rust-ed25519-dalek-2.1.1
                                rust-either-1.13.0
                                rust-elliptic-curve-0.13.8
                                rust-email-lib-0.26.4
                                rust-email-macros-0.0.2
                                rust-email-address-0.2.9
                                rust-encoding-rs-0.8.35
                                rust-endi-1.1.0
                                rust-enum-as-inner-0.6.1
                                rust-enumflags2-0.7.10
                                rust-enumflags2-derive-0.7.10
                                rust-equivalent-1.0.1
                                rust-erased-serde-0.4.5
                                rust-errno-0.3.10
                                rust-event-listener-5.4.0
                                rust-event-listener-strategy-0.5.3
                                rust-eyre-0.6.12
                                rust-fastrand-2.3.0
                                rust-ff-0.13.0
                                rust-fiat-crypto-0.2.9
                                rust-filetime-0.2.25
                                rust-fixedbitset-0.4.2
                                rust-flate2-1.0.35
                                rust-fnv-1.0.7
                                rust-form-urlencoded-1.2.1
                                rust-from-variants-0.6.0
                                rust-from-variants-impl-0.6.0
                                rust-fs2-0.4.3
                                rust-futures-0.3.31
                                rust-futures-channel-0.3.31
                                rust-futures-core-0.3.31
                                rust-futures-executor-0.3.31
                                rust-futures-io-0.3.31
                                rust-futures-lite-2.5.0
                                rust-futures-macro-0.3.31
                                rust-futures-sink-0.3.31
                                rust-futures-task-0.3.31
                                rust-futures-util-0.3.31
                                rust-fuzzy-matcher-0.3.7
                                rust-fxhash-0.2.1
                                rust-generic-array-0.14.7
                                rust-gethostname-0.4.3
                                rust-getrandom-0.2.15
                                rust-gimli-0.28.1
                                rust-git2-0.19.0
                                rust-gpg-error-0.6.2
                                rust-gpgme-0.11.0
                                rust-gpgme-sys-0.11.0
                                rust-group-0.13.0
                                rust-hashbrown-0.14.5
                                rust-hashbrown-0.15.2
                                rust-heck-0.5.0
                                rust-hermit-abi-0.4.0
                                rust-hex-0.4.3
                                rust-hickory-proto-0.24.2
                                rust-hickory-resolver-0.24.2
                                rust-hkdf-0.12.4
                                rust-hmac-0.12.1
                                rust-hostname-0.3.1
                                rust-http-1.2.0
                                rust-http-lib-0.1.0
                                rust-httparse-1.9.5
                                rust-iana-time-zone-0.1.61
                                rust-iana-time-zone-haiku-0.1.2
                                rust-icu-collections-1.5.0
                                rust-icu-locid-1.5.0
                                rust-icu-locid-transform-1.5.0
                                rust-icu-locid-transform-data-1.5.0
                                rust-icu-normalizer-1.5.0
                                rust-icu-normalizer-data-1.5.0
                                rust-icu-properties-1.5.1
                                rust-icu-properties-data-1.5.0
                                rust-icu-provider-1.5.0
                                rust-icu-provider-macros-1.5.0
                                rust-idea-0.5.1
                                rust-ident-case-1.0.1
                                rust-idna-1.0.3
                                rust-idna-adapter-1.2.0
                                rust-imap-client-0.2.3
                                rust-imap-codec-2.0.0-alpha.5
                                rust-imap-next-0.3.1
                                rust-imap-types-2.0.0-alpha.4
                                rust-indenter-0.3.3
                                rust-indexmap-2.7.0
                                rust-inotify-0.9.6
                                rust-inotify-sys-0.1.5
                                rust-inout-0.1.3
                                rust-inquire-0.7.5
                                rust-instant-0.1.13
                                rust-ipconfig-0.3.2
                                rust-ipnet-2.10.1
                                rust-is-docker-0.2.0
                                rust-is-wsl-0.4.0
                                rust-is-terminal-polyfill-1.70.1
                                rust-itoa-1.0.14
                                rust-jni-0.19.0
                                rust-jni-sys-0.3.0
                                rust-jobserver-0.1.32
                                rust-js-sys-0.3.76
                                rust-keccak-0.1.5
                                rust-keyring-3.6.1
                                rust-keyring-lib-1.0.2
                                rust-kqueue-1.0.8
                                rust-kqueue-sys-1.0.4
                                rust-lazy-static-1.5.0
                                rust-libc-0.2.169
                                rust-libdbus-sys-0.2.5
                                rust-libgit2-sys-0.17.0+1.8.1
                                rust-libgpg-error-sys-0.6.2
                                rust-libm-0.2.11
                                rust-libredox-0.1.3
                                rust-libz-sys-1.1.21
                                rust-linked-hash-map-0.5.6
                                rust-linux-keyutils-0.2.4
                                rust-linux-raw-sys-0.4.15
                                rust-litemap-0.7.4
                                rust-lock-api-0.4.12
                                rust-log-0.4.24
                                rust-lru-cache-0.1.2
                                rust-mail-builder-0.3.2
                                rust-mail-parser-0.9.4
                                rust-mail-send-0.4.9
                                rust-maildirs-0.2.2
                                rust-match-cfg-0.1.0
                                rust-matchers-0.1.0
                                rust-md-5-0.10.6
                                rust-md5-0.7.0
                                rust-memchr-2.7.4
                                rust-memoffset-0.7.1
                                rust-memoffset-0.9.1
                                rust-mime-0.3.17
                                rust-mime-guess-2.0.5
                                rust-minimal-lexical-0.2.1
                                rust-miniz-oxide-0.7.4
                                rust-miniz-oxide-0.8.2
                                rust-mio-0.8.11
                                rust-mio-1.0.3
                                rust-mml-lib-1.1.1
                                rust-nanohtml2text-0.1.5
                                rust-newline-converter-0.3.0
                                rust-nix-0.29.0
                                rust-nom-7.1.3
                                rust-notify-6.1.1
                                rust-notmuch-0.8.0
                                rust-nu-ansi-term-0.46.0
                                rust-num-0.4.3
                                rust-num-bigint-0.4.6
                                rust-num-bigint-dig-0.8.4
                                rust-num-complex-0.4.6
                                rust-num-derive-0.4.2
                                rust-num-integer-0.1.46
                                rust-num-iter-0.1.45
                                rust-num-rational-0.4.2
                                rust-num-traits-0.2.19
                                rust-oauth-lib-2.0.0
                                rust-oauth2-5.0.0-rc.1
                                rust-object-0.32.2
                                rust-once-cell-1.20.2
                                rust-open-5.3.2
                                rust-openssl-probe-0.1.5
                                rust-option-ext-0.2.0
                                rust-ordered-stream-0.2.0
                                rust-os-str-bytes-6.6.1
                                rust-ouroboros-0.15.6
                                rust-ouroboros-macro-0.15.6
                                rust-overload-0.1.1
                                rust-owo-colors-3.5.0
                                rust-p256-0.13.2
                                rust-p384-0.13.0
                                rust-parking-2.2.1
                                rust-parking-lot-0.11.2
                                rust-parking-lot-0.12.3
                                rust-parking-lot-core-0.8.6
                                rust-parking-lot-core-0.9.10
                                rust-paste-1.0.15
                                rust-pathdiff-0.2.3
                                rust-pem-rfc7468-0.7.0
                                rust-percent-encoding-2.3.1
                                rust-petgraph-0.6.5
                                rust-pgp-0.10.2
                                rust-pgp-lib-1.0.0
                                rust-pimalaya-tui-0.2.2
                                rust-pin-project-lite-0.2.16
                                rust-pin-utils-0.1.0
                                rust-piper-0.2.4
                                rust-pkcs1-0.7.5
                                rust-pkcs8-0.10.2
                                rust-pkg-config-0.3.31
                                rust-polling-3.7.4
                                rust-ppv-lite86-0.2.20
                                rust-primeorder-0.13.6
                                rust-proc-macro-crate-3.2.0
                                rust-proc-macro-error-1.0.4
                                rust-proc-macro-error-attr-1.0.4
                                rust-proc-macro2-1.0.93
                                rust-process-lib-1.0.0
                                rust-quick-error-1.2.3
                                rust-quote-1.0.38
                                rust-rand-0.8.5
                                rust-rand-chacha-0.3.1
                                rust-rand-core-0.6.4
                                rust-rayon-1.10.0
                                rust-rayon-core-1.12.1
                                rust-redox-syscall-0.2.16
                                rust-redox-syscall-0.5.8
                                rust-redox-users-0.4.6
                                rust-regex-1.11.1
                                rust-regex-automata-0.1.10
                                rust-regex-automata-0.3.9
                                rust-regex-automata-0.4.9
                                rust-regex-syntax-0.6.29
                                rust-regex-syntax-0.7.5
                                rust-regex-syntax-0.8.5
                                rust-resolv-conf-0.7.0
                                rust-rfc6979-0.4.0
                                rust-ring-0.17.8
                                rust-rip-starttls-0.1.0
                                rust-ripemd-0.1.3
                                rust-roff-0.2.2
                                rust-rsa-0.9.7
                                rust-rustc-demangle-0.1.24
                                rust-rustc-version-0.4.1
                                rust-rustix-0.38.43
                                rust-rustls-0.21.12
                                rust-rustls-0.23.21
                                rust-rustls-native-certs-0.7.3
                                rust-rustls-pemfile-1.0.4
                                rust-rustls-pemfile-2.2.0
                                rust-rustls-pki-types-1.10.1
                                rust-rustls-platform-verifier-0.3.4
                                rust-rustls-platform-verifier-0.4.0
                                rust-rustls-platform-verifier-android-0.1.1
                                rust-rustls-webpki-0.101.7
                                rust-rustls-webpki-0.102.8
                                rust-rustversion-1.0.19
                                rust-ryu-1.0.18
                                rust-same-file-1.0.6
                                rust-schannel-0.1.27
                                rust-scopeguard-1.2.0
                                rust-sct-0.7.1
                                rust-sec1-0.7.3
                                rust-secret-lib-1.0.0
                                rust-secret-service-4.0.0
                                rust-security-framework-2.11.1
                                rust-security-framework-3.2.0
                                rust-security-framework-sys-2.14.0
                                rust-semver-1.0.24
                                rust-serde-1.0.217
                                rust-serde-toml-merge-0.3.8
                                rust-serde-xml-rs-0.6.0
                                rust-serde-derive-1.0.217
                                rust-serde-fmt-1.0.3
                                rust-serde-json-1.0.135
                                rust-serde-path-to-error-0.1.16
                                rust-serde-repr-0.1.19
                                rust-serde-spanned-0.6.8
                                rust-sha1-0.10.6
                                rust-sha2-0.10.8
                                rust-sha3-0.10.8
                                rust-sharded-slab-0.1.7
                                rust-shellexpand-3.1.0
                                rust-shellexpand-utils-0.2.1
                                rust-shlex-1.3.0
                                rust-signal-hook-0.3.17
                                rust-signal-hook-mio-0.2.4
                                rust-signal-hook-registry-1.4.2
                                rust-signature-2.2.0
                                rust-slab-0.4.9
                                rust-sled-0.34.7
                                rust-smallvec-1.13.2
                                rust-smtp-proto-0.1.5
                                rust-socket2-0.5.8
                                rust-spin-0.9.8
                                rust-spki-0.7.3
                                rust-stable-deref-trait-1.2.0
                                rust-static-assertions-1.1.0
                                rust-strsim-0.9.3
                                rust-strsim-0.10.0
                                rust-strsim-0.11.1
                                rust-strum-0.26.3
                                rust-strum-macros-0.26.4
                                rust-subtle-2.6.1
                                rust-sval-2.13.2
                                rust-sval-buffer-2.13.2
                                rust-sval-dynamic-2.13.2
                                rust-sval-fmt-2.13.2
                                rust-sval-json-2.13.2
                                rust-sval-nested-2.13.2
                                rust-sval-ref-2.13.2
                                rust-sval-serde-2.13.2
                                rust-syn-1.0.109
                                rust-syn-2.0.96
                                rust-synstructure-0.13.1
                                rust-system-deps-6.2.2
                                rust-target-lexicon-0.12.16
                                rust-tempfile-3.15.0
                                rust-terminal-size-0.4.1
                                rust-thiserror-1.0.69
                                rust-thiserror-2.0.11
                                rust-thiserror-impl-1.0.69
                                rust-thiserror-impl-2.0.11
                                rust-thread-local-1.1.8
                                rust-tinystr-0.7.6
                                rust-tinyvec-1.8.1
                                rust-tinyvec-macros-0.1.1
                                rust-tokio-1.43.0
                                rust-tokio-macros-2.5.0
                                rust-tokio-rustls-0.24.1
                                rust-tokio-rustls-0.26.1
                                rust-toml-0.8.19
                                rust-toml-datetime-0.6.8
                                rust-toml-edit-0.22.22
                                rust-tracing-0.1.41
                                rust-tracing-attributes-0.1.28
                                rust-tracing-core-0.1.33
                                rust-tracing-error-0.2.1
                                rust-tracing-log-0.2.0
                                rust-tracing-subscriber-0.3.19
                                rust-tree-magic-mini-3.1.6
                                rust-twofish-0.7.1
                                rust-typeid-1.0.2
                                rust-typenum-1.17.0
                                rust-uds-windows-1.1.0
                                rust-unicase-2.8.1
                                rust-unicode-ident-1.0.14
                                rust-unicode-segmentation-1.12.0
                                rust-unicode-width-0.1.14
                                rust-unicode-width-0.2.0
                                rust-untrusted-0.9.0
                                rust-ureq-3.0.0-rc5
                                rust-ureq-proto-0.2.3
                                rust-url-2.5.4
                                rust-urlencoding-2.1.3
                                rust-utf-8-0.7.6
                                rust-utf16-iter-1.0.5
                                rust-utf7-imap-0.3.2
                                rust-utf8-iter-1.0.4
                                rust-utf8parse-0.2.2
                                rust-uuid-0.8.2
                                rust-uuid-1.11.1
                                rust-valuable-0.1.0
                                rust-value-bag-1.10.0
                                rust-value-bag-serde1-1.10.0
                                rust-value-bag-sval2-1.10.0
                                rust-vcpkg-0.2.15
                                rust-version-compare-0.2.0
                                rust-version-check-0.9.5
                                rust-walkdir-2.5.0
                                rust-wasi-0.11.0+wasi-snapshot-preview1
                                rust-wasm-bindgen-0.2.99
                                rust-wasm-bindgen-backend-0.2.99
                                rust-wasm-bindgen-macro-0.2.99
                                rust-wasm-bindgen-macro-support-0.2.99
                                rust-wasm-bindgen-shared-0.2.99
                                rust-webpki-root-certs-0.26.7
                                rust-webpki-roots-0.26.7
                                rust-widestring-1.1.0
                                rust-winapi-0.3.9
                                rust-winapi-i686-pc-windows-gnu-0.4.0
                                rust-winapi-util-0.1.9
                                rust-winapi-x86-64-pc-windows-gnu-0.4.0
                                rust-windows-core-0.52.0
                                rust-windows-sys-0.48.0
                                rust-windows-sys-0.52.0
                                rust-windows-sys-0.59.0
                                rust-windows-targets-0.48.5
                                rust-windows-targets-0.52.6
                                rust-windows-aarch64-gnullvm-0.48.5
                                rust-windows-aarch64-gnullvm-0.52.6
                                rust-windows-aarch64-msvc-0.48.5
                                rust-windows-aarch64-msvc-0.52.6
                                rust-windows-i686-gnu-0.48.5
                                rust-windows-i686-gnu-0.52.6
                                rust-windows-i686-gnullvm-0.52.6
                                rust-windows-i686-msvc-0.48.5
                                rust-windows-i686-msvc-0.52.6
                                rust-windows-x86-64-gnu-0.48.5
                                rust-windows-x86-64-gnu-0.52.6
                                rust-windows-x86-64-gnullvm-0.48.5
                                rust-windows-x86-64-gnullvm-0.52.6
                                rust-windows-x86-64-msvc-0.48.5
                                rust-windows-x86-64-msvc-0.52.6
                                rust-winnow-0.6.24
                                rust-winreg-0.10.1
                                rust-winreg-0.50.0
                                rust-winreg-0.52.0
                                rust-write16-1.0.0
                                rust-writeable-0.5.5
                                rust-x25519-dalek-2.0.1
                                rust-xdg-home-1.3.0
                                rust-xml-rs-0.8.25
                                rust-yansi-0.5.1
                                rust-yoke-0.7.5
                                rust-yoke-derive-0.7.5
                                rust-z-base-32-0.1.4
                                rust-zbus-4.4.0
                                rust-zbus-macros-4.4.0
                                rust-zbus-names-3.0.0
                                rust-zerocopy-0.7.35
                                rust-zerocopy-derive-0.7.35
                                rust-zerofrom-0.1.5
                                rust-zerofrom-derive-0.1.5
                                rust-zeroize-1.8.1
                                rust-zeroize-derive-1.4.2
                                rust-zerovec-0.10.4
                                rust-zerovec-derive-0.10.3
                                rust-zvariant-4.2.0
                                rust-zvariant-derive-4.2.0
                                rust-zvariant-utils-2.1.0))
                     (ironbar =>
                              (list rust-addr2line-0.21.0
                               rust-adler-1.0.2
                               rust-ahash-0.8.11
                               rust-aho-corasick-1.1.3
                               rust-android-system-properties-0.1.5
                               rust-anstream-0.6.18
                               rust-anstyle-1.0.10
                               rust-anstyle-parse-0.2.6
                               rust-anstyle-query-1.1.2
                               rust-anstyle-wincon-3.0.7
                               rust-arraydeque-0.5.1
                               rust-async-broadcast-0.7.2
                               rust-async-io-2.4.0
                               rust-async-lock-3.4.0
                               rust-async-pidfd-0.1.5
                               rust-async-recursion-1.1.1
                               rust-async-stream-0.3.6
                               rust-async-stream-impl-0.3.6
                               rust-async-trait-0.1.87
                               rust-atomic-waker-1.1.2
                               rust-autocfg-1.4.0
                               rust-backtrace-0.3.71
                               rust-base64-0.22.1
                               rust-bitflags-1.3.2
                               rust-bitflags-2.9.1
                               rust-block-buffer-0.10.4
                               rust-block2-0.6.2
                               rust-bluer-0.17.4
                               rust-bstr-1.11.3
                               rust-bumpalo-3.17.0
                               rust-bytes-1.10.0
                               rust-cairo-rs-0.21.2
                               rust-cairo-sys-rs-0.21.2
                               rust-calloop-0.14.3
                               rust-calloop-wayland-source-0.4.1
                               rust-cc-1.2.16
                               rust-cfg-expr-0.17.2
                               rust-cfg-if-1.0.4
                               rust-cfg-aliases-0.2.1
                               rust-chrono-0.4.42
                               rust-clap-4.5.53
                               rust-clap-builder-4.5.53
                               rust-clap-complete-4.5.61
                               rust-clap-derive-4.5.49
                               rust-clap-lex-0.7.4
                               rust-color-eyre-0.6.5
                               rust-color-spantrace-0.3.0
                               rust-colorchoice-1.0.3
                               rust-colpetto-0.7.0
                               rust-concurrent-queue-2.5.0
                               rust-config-0.15.19
                               rust-core-foundation-0.9.4
                               rust-core-foundation-sys-0.8.7
                               rust-cpufeatures-0.2.17
                               rust-crossbeam-channel-0.5.15
                               rust-crossbeam-utils-0.8.21
                               rust-crypto-common-0.1.6
                               rust-ctrlc-3.5.1
                               rust-cursor-icon-1.2.0
                               rust-custom-debug-0.6.2
                               rust-custom-debug-derive-0.6.2
                               rust-darling-0.14.4
                               rust-darling-0.20.11
                               rust-darling-core-0.14.4
                               rust-darling-core-0.20.11
                               rust-darling-macro-0.14.4
                               rust-darling-macro-0.20.11
                               rust-dbus-0.9.7
                               rust-dbus-crossroads-0.5.2
                               rust-dbus-tokio-0.7.6
                               rust-deranged-0.3.11
                               rust-derive-is-enum-variant-0.1.1
                               rust-derive-more-2.0.1
                               rust-derive-more-impl-2.0.1
                               rust-devil-0.0.7
                               rust-digest-0.10.7
                               rust-dirs-6.0.0
                               rust-dirs-sys-0.5.0
                               rust-discard-1.0.4
                               rust-dispatch2-0.3.0
                               rust-displaydoc-0.2.5
                               rust-downcast-rs-1.2.1
                               rust-dyn-clone-1.0.18
                               rust-either-1.15.0
                               rust-encoding-rs-0.8.35
                               rust-endi-1.1.0
                               rust-enum-kinds-0.5.1
                               rust-enumflags2-0.7.11
                               rust-enumflags2-derive-0.7.11
                               rust-equivalent-1.0.2
                               rust-errno-0.3.10
                               rust-evdev-rs-0.6.3
                               rust-evdev-sys-0.2.6
                               rust-event-listener-5.4.0
                               rust-event-listener-strategy-0.5.3
                               rust-eyre-0.6.12
                               rust-fastrand-2.3.0
                               rust-field-offset-0.3.6
                               rust-fnv-1.0.7
                               rust-foldhash-0.1.5
                               rust-foreign-types-0.3.2
                               rust-foreign-types-shared-0.1.1
                               rust-form-urlencoded-1.2.1
                               rust-from-variants-1.0.2
                               rust-from-variants-impl-1.0.2
                               rust-futures-0.3.31
                               rust-futures-channel-0.3.31
                               rust-futures-core-0.3.31
                               rust-futures-executor-0.3.31
                               rust-futures-io-0.3.31
                               rust-futures-lite-2.6.1
                               rust-futures-macro-0.3.31
                               rust-futures-signals-0.3.34
                               rust-futures-sink-0.3.31
                               rust-futures-task-0.3.31
                               rust-futures-util-0.3.31
                               rust-gdk-pixbuf-0.21.2
                               rust-gdk-pixbuf-sys-0.21.2
                               rust-gdk4-0.10.1
                               rust-gdk4-sys-0.10.1
                               rust-generic-array-0.14.9
                               rust-gensym-0.1.1
                               rust-getrandom-0.2.15
                               rust-getrandom-0.3.1
                               rust-gimli-0.28.1
                               rust-gio-0.21.2
                               rust-gio-sys-0.21.2
                               rust-gl-0.14.0
                               rust-gl-generator-0.14.0
                               rust-glib-0.21.3
                               rust-glib-macros-0.21.2
                               rust-glib-sys-0.21.2
                               rust-gobject-sys-0.21.2
                               rust-graphene-rs-0.21.2
                               rust-graphene-sys-0.21.2
                               rust-gsk4-0.10.1
                               rust-gsk4-sys-0.10.1
                               rust-gtk4-0.10.1
                               rust-gtk4-layer-shell-0.6.3
                               rust-gtk4-layer-shell-sys-0.4.0
                               rust-gtk4-macros-0.10.1
                               rust-gtk4-sys-0.10.1
                               rust-h2-0.4.8
                               rust-hashbrown-0.15.5
                               rust-hashbrown-0.16.1
                               rust-hashlink-0.10.0
                               rust-heck-0.3.3
                               rust-heck-0.5.0
                               rust-hermit-abi-0.4.0
                               rust-hex-0.4.3
                               rust-http-1.2.0
                               rust-http-body-1.0.1
                               rust-http-body-util-0.1.2
                               rust-httparse-1.10.1
                               rust-hyper-1.6.0
                               rust-hyper-rustls-0.27.5
                               rust-hyper-tls-0.6.0
                               rust-hyper-util-0.1.13
                               rust-hyprland-0.4.0-beta.3
                               rust-hyprland-macros-0.4.0-beta.3
                               rust-iana-time-zone-0.1.61
                               rust-iana-time-zone-haiku-0.1.2
                               rust-icu-collections-1.5.0
                               rust-icu-locid-1.5.0
                               rust-icu-locid-transform-1.5.0
                               rust-icu-locid-transform-data-1.5.0
                               rust-icu-normalizer-1.5.0
                               rust-icu-normalizer-data-1.5.0
                               rust-icu-properties-1.5.1
                               rust-icu-properties-data-1.5.0
                               rust-icu-provider-1.5.0
                               rust-icu-provider-macros-1.5.0
                               rust-ident-case-1.0.1
                               rust-idna-1.0.3
                               rust-idna-adapter-1.2.0
                               rust-indenter-0.3.3
                               rust-indexmap-2.12.1
                               rust-inotify-0.11.0
                               rust-inotify-sys-0.1.5
                               rust-ipnet-2.11.0
                               rust-iri-string-0.7.8
                               rust-is-terminal-polyfill-1.70.1
                               rust-itoa-1.0.14
                               rust-js-sys-0.3.77
                               rust-khronos-api-3.1.0
                               rust-kqueue-1.1.1
                               rust-kqueue-sys-1.0.4
                               rust-lazy-static-1.5.0
                               rust-libc-0.2.174
                               rust-libcorn-0.10.0
                               rust-libdbus-sys-0.2.5
                               rust-libpulse-binding-2.30.1
                               rust-libpulse-sys-1.22.0
                               rust-libredox-0.1.3
                               rust-linux-raw-sys-0.4.15
                               rust-linux-raw-sys-0.11.0
                               rust-litemap-0.7.5
                               rust-lock-api-0.4.12
                               rust-log-0.4.27
                               rust-lua-src-548.1.2
                               rust-macaddr-1.0.1
                               rust-matchers-0.1.0
                               rust-memchr-2.7.6
                               rust-memmap2-0.9.5
                               rust-memoffset-0.9.1
                               rust-minimal-lexical-0.2.1
                               rust-miniz-oxide-0.7.4
                               rust-mio-1.0.3
                               rust-mlua-0.11.5
                               rust-mlua-sys-0.9.0
                               rust-mpd-utils-0.2.1
                               rust-mpd-client-1.4.1
                               rust-mpd-protocol-1.0.3
                               rust-mpris-2.0.1
                               rust-native-tls-0.2.14
                               rust-nix-0.29.0
                               rust-nix-0.30.1
                               rust-nom-7.1.3
                               rust-notify-8.2.0
                               rust-notify-types-2.0.0
                               rust-ntapi-0.4.1
                               rust-nu-ansi-term-0.46.0
                               rust-num-conv-0.1.0
                               rust-num-derive-0.4.2
                               rust-num-traits-0.2.19
                               rust-objc2-0.6.3
                               rust-objc2-core-foundation-0.3.1
                               rust-objc2-encode-4.1.0
                               rust-objc2-io-kit-0.3.1
                               rust-object-0.32.2
                               rust-once-cell-1.20.3
                               rust-openssl-0.10.72
                               rust-openssl-macros-0.1.1
                               rust-openssl-probe-0.1.6
                               rust-openssl-sys-0.9.107
                               rust-option-ext-0.2.0
                               rust-ordered-stream-0.2.0
                               rust-overload-0.1.1
                               rust-owo-colors-4.2.1
                               rust-pango-0.21.3
                               rust-pango-sys-0.21.2
                               rust-parking-2.2.1
                               rust-parking-lot-0.12.3
                               rust-parking-lot-core-0.9.10
                               rust-paste-1.0.15
                               rust-pastey-0.1.1
                               rust-pathdiff-0.2.3
                               rust-percent-encoding-2.3.1
                               rust-pest-2.8.3
                               rust-pest-derive-2.8.3
                               rust-pest-generator-2.8.3
                               rust-pest-meta-2.8.3
                               rust-pin-project-1.1.10
                               rust-pin-project-internal-1.1.10
                               rust-pin-project-lite-0.2.16
                               rust-pin-utils-0.1.0
                               rust-pkg-config-0.3.32
                               rust-polling-3.7.4
                               rust-powerfmt-0.2.0
                               rust-proc-macro-crate-3.4.0
                               rust-proc-macro2-1.0.94
                               rust-pure-rust-locales-0.8.1
                               rust-quick-xml-0.37.2
                               rust-quote-0.3.15
                               rust-quote-1.0.39
                               rust-redox-syscall-0.5.10
                               rust-redox-users-0.5.0
                               rust-ref-cast-1.0.24
                               rust-ref-cast-impl-1.0.24
                               rust-regex-1.12.2
                               rust-regex-automata-0.1.10
                               rust-regex-automata-0.4.13
                               rust-regex-syntax-0.6.29
                               rust-regex-syntax-0.8.5
                               rust-reqwest-0.12.25
                               rust-ring-0.17.14
                               rust-rustc-demangle-0.1.24
                               rust-rustc-hash-2.1.1
                               rust-rustc-version-0.4.1
                               rust-rustix-0.38.44
                               rust-rustix-1.1.2
                               rust-rustls-0.23.23
                               rust-rustls-pki-types-1.11.0
                               rust-rustls-webpki-0.102.8
                               rust-rustversion-1.0.20
                               rust-ryu-1.0.19
                               rust-same-file-1.0.6
                               rust-schannel-0.1.27
                               rust-schemars-1.1.0
                               rust-schemars-derive-1.1.0
                               rust-scopeguard-1.2.0
                               rust-security-framework-2.11.1
                               rust-security-framework-sys-2.14.0
                               rust-semver-1.0.25
                               rust-serde-1.0.228
                               rust-serde-core-1.0.228
                               rust-serde-derive-1.0.228
                               rust-serde-derive-internals-0.29.1
                               rust-serde-json-1.0.145
                               rust-serde-repr-0.1.19
                               rust-serde-spanned-0.6.8
                               rust-serde-spanned-1.0.3
                               rust-serde-urlencoded-0.7.1
                               rust-sha2-0.10.9
                               rust-sharded-slab-0.1.7
                               rust-shlex-1.3.0
                               rust-signal-hook-registry-1.4.2
                               rust-slab-0.4.9
                               rust-smallvec-1.15.1
                               rust-smithay-client-toolkit-0.20.0
                               rust-socket2-0.5.10
                               rust-socket2-0.6.0
                               rust-stable-deref-trait-1.2.0
                               rust-static-assertions-1.1.0
                               rust-strip-ansi-escapes-0.2.1
                               rust-strsim-0.10.0
                               rust-strsim-0.11.1
                               rust-strum-0.26.3
                               rust-strum-macros-0.26.4
                               rust-subtle-2.6.1
                               rust-swayipc-async-3.0.0
                               rust-swayipc-types-2.0.1
                               rust-syn-0.11.11
                               rust-syn-1.0.109
                               rust-syn-2.0.106
                               rust-sync-wrapper-1.0.2
                               rust-synom-0.11.3
                               rust-synstructure-0.13.1
                               rust-sysinfo-0.37.2
                               rust-system-deps-7.0.3
                               rust-system-tray-0.8.4
                               rust-target-lexicon-0.12.16
                               rust-tempfile-3.17.1
                               rust-thiserror-1.0.69
                               rust-thiserror-2.0.17
                               rust-thiserror-impl-1.0.69
                               rust-thiserror-impl-2.0.17
                               rust-thread-local-1.1.8
                               rust-time-0.3.37
                               rust-time-core-0.1.2
                               rust-time-macros-0.2.19
                               rust-tinystr-0.7.6
                               rust-tokio-1.48.0
                               rust-tokio-macros-2.6.0
                               rust-tokio-native-tls-0.3.1
                               rust-tokio-rustls-0.26.2
                               rust-tokio-stream-0.1.17
                               rust-tokio-util-0.7.13
                               rust-toml-0.8.20
                               rust-toml-0.9.8
                               rust-toml-datetime-0.6.8
                               rust-toml-datetime-0.7.3
                               rust-toml-edit-0.22.24
                               rust-toml-edit-0.23.6
                               rust-toml-parser-1.0.4
                               rust-tower-0.5.2
                               rust-tower-http-0.6.8
                               rust-tower-layer-0.3.3
                               rust-tower-service-0.3.3
                               rust-tracing-0.1.43
                               rust-tracing-appender-0.2.4
                               rust-tracing-attributes-0.1.31
                               rust-tracing-core-0.1.35
                               rust-tracing-error-0.2.1
                               rust-tracing-log-0.2.0
                               rust-tracing-subscriber-0.3.19
                               rust-try-lock-0.2.5
                               rust-typenum-1.19.0
                               rust-ucd-trie-0.1.7
                               rust-uds-windows-1.1.0
                               rust-unicode-ident-1.0.17
                               rust-unicode-segmentation-1.12.0
                               rust-unicode-xid-0.0.4
                               rust-unicode-xid-0.2.6
                               rust-untrusted-0.9.0
                               rust-url-2.5.4
                               rust-utf16-iter-1.0.5
                               rust-utf8-iter-1.0.4
                               rust-utf8parse-0.2.2
                               rust-uuid-1.15.1
                               rust-valuable-0.1.1
                               rust-vcpkg-0.2.15
                               rust-version-compare-0.2.0
                               rust-version-check-0.9.5
                               rust-vte-0.14.1
                               rust-walkdir-2.5.0
                               rust-want-0.3.1
                               rust-wasi-0.11.0+wasi-snapshot-preview1
                               rust-wasi-0.13.3+wasi-0.2.2
                               rust-wasm-bindgen-0.2.100
                               rust-wasm-bindgen-backend-0.2.100
                               rust-wasm-bindgen-futures-0.4.50
                               rust-wasm-bindgen-macro-0.2.100
                               rust-wasm-bindgen-macro-support-0.2.100
                               rust-wasm-bindgen-shared-0.2.100
                               rust-wayland-backend-0.3.11
                               rust-wayland-client-0.31.11
                               rust-wayland-csd-frame-0.3.0
                               rust-wayland-cursor-0.31.8
                               rust-wayland-protocols-0.32.9
                               rust-wayland-protocols-experimental-20250721.0.1
                               rust-wayland-protocols-misc-0.3.9
                               rust-wayland-protocols-wlr-0.3.9
                               rust-wayland-scanner-0.31.7
                               rust-wayland-sys-0.31.7
                               rust-web-sys-0.3.77
                               rust-winapi-0.3.9
                               rust-winapi-i686-pc-windows-gnu-0.4.0
                               rust-winapi-util-0.1.9
                               rust-winapi-x86-64-pc-windows-gnu-0.4.0
                               rust-windows-0.60.0
                               rust-windows-collections-0.1.1
                               rust-windows-core-0.52.0
                               rust-windows-core-0.60.1
                               rust-windows-future-0.1.1
                               rust-windows-implement-0.59.0
                               rust-windows-interface-0.59.1
                               rust-windows-link-0.1.0
                               rust-windows-link-0.2.0
                               rust-windows-numerics-0.1.1
                               rust-windows-result-0.3.1
                               rust-windows-strings-0.3.1
                               rust-windows-sys-0.52.0
                               rust-windows-sys-0.59.0
                               rust-windows-sys-0.60.2
                               rust-windows-sys-0.61.0
                               rust-windows-targets-0.52.6
                               rust-windows-targets-0.53.2
                               rust-windows-aarch64-gnullvm-0.52.6
                               rust-windows-aarch64-gnullvm-0.53.0
                               rust-windows-aarch64-msvc-0.52.6
                               rust-windows-aarch64-msvc-0.53.0
                               rust-windows-i686-gnu-0.52.6
                               rust-windows-i686-gnu-0.53.0
                               rust-windows-i686-gnullvm-0.52.6
                               rust-windows-i686-gnullvm-0.53.0
                               rust-windows-i686-msvc-0.52.6
                               rust-windows-i686-msvc-0.53.0
                               rust-windows-x86-64-gnu-0.52.6
                               rust-windows-x86-64-gnu-0.53.0
                               rust-windows-x86-64-gnullvm-0.52.6
                               rust-windows-x86-64-gnullvm-0.53.0
                               rust-windows-x86-64-msvc-0.52.6
                               rust-windows-x86-64-msvc-0.53.0
                               rust-winnow-0.7.13
                               rust-wit-bindgen-rt-0.33.0
                               rust-write16-1.0.0
                               rust-writeable-0.5.5
                               rust-xcursor-0.3.8
                               rust-xkeysym-0.2.1
                               rust-xml-rs-0.8.27
                               rust-yaml-rust2-0.10.4
                               rust-yoke-0.7.5
                               rust-yoke-derive-0.7.5
                               rust-zbus-5.12.0
                               rust-zbus-macros-5.12.0
                               rust-zbus-names-4.2.0
                               rust-zerocopy-0.7.35
                               rust-zerocopy-derive-0.7.35
                               rust-zerofrom-0.1.6
                               rust-zerofrom-derive-0.1.6
                               rust-zeroize-1.8.1
                               rust-zerovec-0.10.4
                               rust-zerovec-derive-0.10.3
                               rust-zvariant-5.5.3
                               rust-zvariant-derive-5.5.3
                               rust-zvariant-utils-3.2.0))
                     (jj-cli =>
                             (list rust-addr2line-0.24.2
                                   rust-adler2-2.0.0
                                   rust-ahash-0.8.11
                                   rust-aho-corasick-1.1.3
                                   rust-allocator-api2-0.2.21
                                   rust-android-tzdata-0.1.1
                                   rust-android-system-properties-0.1.5
                                   rust-anes-0.1.6
                                   rust-anstream-0.6.15
                                   rust-anstyle-1.0.8
                                   rust-anstyle-parse-0.2.5
                                   rust-anstyle-query-1.1.1
                                   rust-anstyle-wincon-3.0.4
                                   rust-anyhow-1.0.97
                                   rust-arc-swap-1.7.1
                                   rust-arrayvec-0.7.6
                                   rust-assert-cmd-2.0.16
                                   rust-assert-matches-1.5.0
                                   rust-async-trait-0.1.88
                                   rust-atomic-0.6.0
                                   rust-autocfg-1.4.0
                                   rust-backtrace-0.3.74
                                   rust-base64-0.21.7
                                   rust-beef-0.5.2
                                   rust-bit-set-0.5.3
                                   rust-bit-vec-0.6.3
                                   rust-bitflags-1.3.2
                                   rust-bitflags-2.9.0
                                   rust-blake2-0.10.6
                                   rust-block-buffer-0.10.4
                                   rust-bstr-1.11.3
                                   rust-bumpalo-3.16.0
                                   rust-bytemuck-1.21.0
                                   rust-byteorder-1.5.0
                                   rust-bytes-1.10.1
                                   rust-cassowary-0.3.0
                                   rust-cast-0.3.0
                                   rust-castaway-0.2.3
                                   rust-cc-1.2.18
                                   rust-cfg-if-1.0.0
                                   rust-cfg-aliases-0.1.1
                                   rust-chrono-0.4.40
                                   rust-ciborium-0.2.2
                                   rust-ciborium-io-0.2.2
                                   rust-ciborium-ll-0.2.2
                                   rust-clap-4.5.35
                                   rust-clap-markdown-0.1.4
                                   rust-clap-builder-4.5.35
                                   rust-clap-complete-4.5.47
                                   rust-clap-complete-nushell-4.5.5
                                   rust-clap-derive-4.5.32
                                   rust-clap-lex-0.7.4
                                   rust-clap-mangen-0.2.25
                                   rust-clru-0.6.2
                                   rust-cmake-0.1.54
                                   rust-colorchoice-1.0.2
                                   rust-compact-str-0.8.1
                                   rust-console-0.15.8
                                   rust-core-foundation-sys-0.8.7
                                   rust-cpufeatures-0.2.17
                                   rust-crc32fast-1.4.2
                                   rust-criterion-0.5.1
                                   rust-criterion-plot-0.5.0
                                   rust-crossbeam-channel-0.5.14
                                   rust-crossbeam-deque-0.8.5
                                   rust-crossbeam-epoch-0.9.18
                                   rust-crossbeam-utils-0.8.21
                                   rust-crossterm-0.28.1
                                   rust-crossterm-winapi-0.9.1
                                   rust-crunchy-0.2.2
                                   rust-crypto-common-0.1.6
                                   rust-csscolorparser-0.6.2
                                   rust-darling-0.20.10
                                   rust-darling-core-0.20.10
                                   rust-darling-macro-0.20.10
                                   rust-dashmap-6.1.0
                                   rust-deltae-0.3.2
                                   rust-difflib-0.4.0
                                   rust-digest-0.10.7
                                   rust-dirs-6.0.0
                                   rust-dirs-sys-0.5.0
                                   rust-displaydoc-0.2.5
                                   rust-doc-comment-0.3.3
                                   rust-dunce-1.0.5
                                   rust-either-1.15.0
                                   rust-encode-unicode-0.3.6
                                   rust-encoding-rs-0.8.35
                                   rust-enum-dispatch-0.3.13
                                   rust-equivalent-1.0.1
                                   rust-errno-0.3.11
                                   rust-euclid-0.22.11
                                   rust-fancy-regex-0.11.0
                                   rust-faster-hex-0.9.0
                                   rust-fastrand-2.3.0
                                   rust-filedescriptor-0.8.2
                                   rust-filetime-0.2.25
                                   rust-finl-unicode-1.3.0
                                   rust-fixedbitset-0.4.2
                                   rust-flate2-1.1.1
                                   rust-fnv-1.0.7
                                   rust-foldhash-0.1.5
                                   rust-form-urlencoded-1.2.1
                                   rust-futures-0.1.31
                                   rust-futures-0.3.31
                                   rust-futures-channel-0.3.31
                                   rust-futures-core-0.3.31
                                   rust-futures-executor-0.3.31
                                   rust-futures-io-0.3.31
                                   rust-futures-macro-0.3.31
                                   rust-futures-sink-0.3.31
                                   rust-futures-task-0.3.31
                                   rust-futures-util-0.3.31
                                   rust-generic-array-0.14.7
                                   rust-getrandom-0.2.15
                                   rust-getrandom-0.3.2
                                   rust-gimli-0.31.1
                                   rust-git2-0.20.1
                                   rust-gix-0.71.0
                                   rust-gix-actor-0.34.0
                                   rust-gix-attributes-0.25.0
                                   rust-gix-bitmap-0.2.14
                                   rust-gix-chunk-0.4.11
                                   rust-gix-command-0.5.0
                                   rust-gix-commitgraph-0.27.0
                                   rust-gix-config-0.44.0
                                   rust-gix-config-value-0.14.12
                                   rust-gix-date-0.9.4
                                   rust-gix-diff-0.51.0
                                   rust-gix-discover-0.39.0
                                   rust-gix-features-0.41.1
                                   rust-gix-filter-0.18.0
                                   rust-gix-fs-0.14.0
                                   rust-gix-glob-0.19.0
                                   rust-gix-hash-0.17.0
                                   rust-gix-hashtable-0.8.0
                                   rust-gix-ignore-0.14.0
                                   rust-gix-index-0.39.0
                                   rust-gix-lock-17.0.0
                                   rust-gix-object-0.48.0
                                   rust-gix-odb-0.68.0
                                   rust-gix-pack-0.58.0
                                   rust-gix-packetline-0.18.4
                                   rust-gix-packetline-blocking-0.18.3
                                   rust-gix-path-0.10.15
                                   rust-gix-pathspec-0.10.0
                                   rust-gix-protocol-0.49.0
                                   rust-gix-quote-0.5.0
                                   rust-gix-ref-0.51.0
                                   rust-gix-refspec-0.29.0
                                   rust-gix-revision-0.33.0
                                   rust-gix-revwalk-0.19.0
                                   rust-gix-sec-0.10.12
                                   rust-gix-shallow-0.3.0
                                   rust-gix-submodule-0.18.0
                                   rust-gix-tempfile-17.0.0
                                   rust-gix-trace-0.1.12
                                   rust-gix-transport-0.46.0
                                   rust-gix-traverse-0.45.0
                                   rust-gix-url-0.30.0
                                   rust-gix-utils-0.2.0
                                   rust-gix-validate-0.9.4
                                   rust-gix-worktree-0.40.0
                                   rust-glob-0.3.2
                                   rust-globset-0.4.15
                                   rust-half-2.4.1
                                   rust-hashbrown-0.14.5
                                   rust-hashbrown-0.15.2
                                   rust-heck-0.5.0
                                   rust-hermit-abi-0.4.0
                                   rust-hex-0.4.3
                                   rust-home-0.5.11
                                   rust-iana-time-zone-0.1.60
                                   rust-iana-time-zone-haiku-0.1.2
                                   rust-icu-collections-1.5.0
                                   rust-icu-locid-1.5.0
                                   rust-icu-locid-transform-1.5.0
                                   rust-icu-locid-transform-data-1.5.1
                                   rust-icu-normalizer-1.5.0
                                   rust-icu-normalizer-data-1.5.1
                                   rust-icu-properties-1.5.1
                                   rust-icu-properties-data-1.5.1
                                   rust-icu-provider-1.5.0
                                   rust-icu-provider-macros-1.5.0
                                   rust-ident-case-1.0.1
                                   rust-idna-1.0.3
                                   rust-idna-adapter-1.2.0
                                   rust-ignore-0.4.23
                                   rust-imara-diff-0.1.8
                                   rust-indexmap-2.8.0
                                   rust-indoc-2.0.6
                                   rust-insta-1.42.2
                                   rust-instability-0.3.6
                                   rust-interim-0.2.1
                                   rust-is-terminal-0.4.13
                                   rust-is-executable-1.0.4
                                   rust-is-terminal-polyfill-1.70.1
                                   rust-itertools-0.10.5
                                   rust-itertools-0.12.1
                                   rust-itertools-0.13.0
                                   rust-itoa-1.0.15
                                   rust-jiff-0.2.5
                                   rust-jiff-static-0.2.5
                                   rust-jiff-tzdb-0.1.4
                                   rust-jiff-tzdb-platform-0.1.3
                                   rust-jj-lib-0.28.2
                                   rust-jj-lib-proc-macros-0.28.2
                                   rust-jobserver-0.1.33
                                   rust-js-sys-0.3.70
                                   rust-kstring-2.0.2
                                   rust-lab-0.11.0
                                   rust-lazy-static-1.5.0
                                   rust-libc-0.2.171
                                   rust-libgit2-sys-0.18.1+1.9.0
                                   rust-libredox-0.1.3
                                   rust-libssh2-sys-0.3.0
                                   rust-libz-ng-sys-1.1.22
                                   rust-libz-sys-1.1.20
                                   rust-linked-hash-map-0.5.6
                                   rust-linux-raw-sys-0.4.15
                                   rust-linux-raw-sys-0.9.3
                                   rust-litemap-0.7.5
                                   rust-lock-api-0.4.12
                                   rust-log-0.4.27
                                   rust-logos-0.15.0
                                   rust-logos-codegen-0.15.0
                                   rust-logos-derive-0.15.0
                                   rust-lru-0.12.4
                                   rust-mac-address-1.1.7
                                   rust-maplit-1.0.2
                                   rust-matchers-0.1.0
                                   rust-maybe-async-0.2.10
                                   rust-memchr-2.7.4
                                   rust-memmap2-0.5.10
                                   rust-memmap2-0.9.5
                                   rust-memmem-0.1.1
                                   rust-memoffset-0.9.1
                                   rust-minimal-lexical-0.2.1
                                   rust-miniz-oxide-0.8.7
                                   rust-mio-1.0.3
                                   rust-nix-0.28.0
                                   rust-nom-7.1.3
                                   rust-nu-ansi-term-0.46.0
                                   rust-num-derive-0.4.2
                                   rust-num-traits-0.2.19
                                   rust-object-0.36.7
                                   rust-once-cell-1.21.3
                                   rust-oorandom-11.1.4
                                   rust-openssl-probe-0.1.5
                                   rust-openssl-src-300.3.2+3.3.2
                                   rust-openssl-sys-0.9.103
                                   rust-option-ext-0.2.0
                                   rust-ordered-float-4.6.0
                                   rust-os-pipe-1.2.1
                                   rust-overload-0.1.1
                                   rust-parking-lot-0.12.3
                                   rust-parking-lot-core-0.9.10
                                   rust-paste-1.0.15
                                   rust-percent-encoding-2.3.1
                                   rust-pest-2.8.0
                                   rust-pest-derive-2.8.0
                                   rust-pest-generator-2.8.0
                                   rust-pest-meta-2.8.0
                                   rust-phf-0.11.3
                                   rust-phf-codegen-0.11.3
                                   rust-phf-generator-0.11.3
                                   rust-phf-macros-0.11.3
                                   rust-phf-shared-0.11.3
                                   rust-pin-project-1.1.8
                                   rust-pin-project-internal-1.1.8
                                   rust-pin-project-lite-0.2.16
                                   rust-pin-utils-0.1.0
                                   rust-pkg-config-0.3.30
                                   rust-plotters-0.3.6
                                   rust-plotters-backend-0.3.6
                                   rust-plotters-svg-0.3.6
                                   rust-pollster-0.4.0
                                   rust-portable-atomic-1.11.0
                                   rust-portable-atomic-util-0.2.4
                                   rust-ppv-lite86-0.2.20
                                   rust-predicates-3.1.2
                                   rust-predicates-core-1.0.8
                                   rust-predicates-tree-1.0.11
                                   rust-proc-macro2-1.0.94
                                   rust-prodash-29.0.1
                                   rust-prost-0.12.6
                                   rust-prost-derive-0.12.6
                                   rust-quote-1.0.40
                                   rust-r-efi-5.2.0
                                   rust-rand-0.8.5
                                   rust-rand-chacha-0.3.1
                                   rust-rand-core-0.6.4
                                   rust-ratatui-0.29.0
                                   rust-rayon-1.10.0
                                   rust-rayon-core-1.12.1
                                   rust-redox-syscall-0.5.11
                                   rust-redox-users-0.5.0
                                   rust-ref-cast-1.0.24
                                   rust-ref-cast-impl-1.0.24
                                   rust-regex-1.11.1
                                   rust-regex-automata-0.1.10
                                   rust-regex-automata-0.4.9
                                   rust-regex-syntax-0.6.29
                                   rust-regex-syntax-0.8.5
                                   rust-roff-0.2.2
                                   rust-rpassword-7.3.1
                                   rust-rtoolbox-0.0.2
                                   rust-rustc-demangle-0.1.24
                                   rust-rustc-version-0.4.1
                                   rust-rustix-0.38.44
                                   rust-rustix-1.0.5
                                   rust-rustversion-1.0.17
                                   rust-ryu-1.0.18
                                   rust-same-file-1.0.6
                                   rust-sapling-renderdag-0.1.0
                                   rust-sapling-streampager-0.11.0
                                   rust-scm-record-0.8.0
                                   rust-scopeguard-1.2.0
                                   rust-semver-1.0.26
                                   rust-serde-1.0.219
                                   rust-serde-bser-0.4.0
                                   rust-serde-bytes-0.11.15
                                   rust-serde-derive-1.0.219
                                   rust-serde-json-1.0.140
                                   rust-serde-spanned-0.6.8
                                   rust-sha1-0.10.6
                                   rust-sha1-checked-0.10.0
                                   rust-sha2-0.10.8
                                   rust-sharded-slab-0.1.7
                                   rust-shell-words-1.1.0
                                   rust-shlex-1.3.0
                                   rust-signal-hook-0.3.17
                                   rust-signal-hook-mio-0.2.4
                                   rust-signal-hook-registry-1.4.2
                                   rust-similar-2.6.0
                                   rust-siphasher-0.3.11
                                   rust-siphasher-1.0.1
                                   rust-slab-0.4.9
                                   rust-smallvec-1.15.0
                                   rust-smawk-0.3.2
                                   rust-socket2-0.5.9
                                   rust-stable-deref-trait-1.2.0
                                   rust-static-assertions-1.1.0
                                   rust-strsim-0.11.1
                                   rust-strum-0.26.3
                                   rust-strum-macros-0.26.4
                                   rust-subtle-2.6.1
                                   rust-syn-1.0.109
                                   rust-syn-2.0.100
                                   rust-synstructure-0.13.1
                                   rust-tempfile-3.19.1
                                   rust-terminal-size-0.4.0
                                   rust-terminfo-0.9.0
                                   rust-termios-0.3.3
                                   rust-termtree-0.4.1
                                   rust-termwiz-0.23.0
                                   rust-test-case-3.3.1
                                   rust-test-case-core-3.3.1
                                   rust-test-case-macros-3.3.1
                                   rust-textwrap-0.16.2
                                   rust-thiserror-1.0.69
                                   rust-thiserror-2.0.12
                                   rust-thiserror-impl-1.0.69
                                   rust-thiserror-impl-2.0.12
                                   rust-thread-local-1.1.8
                                   rust-timeago-0.4.2
                                   rust-tinystr-0.7.6
                                   rust-tinytemplate-1.2.1
                                   rust-tinyvec-1.9.0
                                   rust-tinyvec-macros-0.1.1
                                   rust-tokio-1.44.2
                                   rust-tokio-macros-2.5.0
                                   rust-tokio-util-0.6.10
                                   rust-toml-0.8.19
                                   rust-toml-datetime-0.6.8
                                   rust-toml-edit-0.22.24
                                   rust-tracing-0.1.41
                                   rust-tracing-attributes-0.1.28
                                   rust-tracing-chrome-0.7.2
                                   rust-tracing-core-0.1.33
                                   rust-tracing-log-0.2.0
                                   rust-tracing-subscriber-0.3.19
                                   rust-typenum-1.18.0
                                   rust-ucd-trie-0.1.6
                                   rust-uluru-3.1.0
                                   rust-unicode-bom-2.0.3
                                   rust-unicode-ident-1.0.18
                                   rust-unicode-linebreak-0.1.5
                                   rust-unicode-normalization-0.1.24
                                   rust-unicode-segmentation-1.12.0
                                   rust-unicode-truncate-1.1.0
                                   rust-unicode-width-0.1.12
                                   rust-unicode-width-0.2.0
                                   rust-url-2.5.4
                                   rust-utf16-iter-1.0.5
                                   rust-utf8-iter-1.0.4
                                   rust-utf8parse-0.2.2
                                   rust-uuid-1.11.1
                                   rust-valuable-0.1.0
                                   rust-vcpkg-0.2.15
                                   rust-vec-map-0.8.2
                                   rust-version-check-0.9.5
                                   rust-vtparse-0.6.2
                                   rust-wait-timeout-0.2.0
                                   rust-walkdir-2.5.0
                                   rust-wasi-0.11.0+wasi-snapshot-preview1
                                   rust-wasi-0.14.2+wasi-0.2.4
                                   rust-wasite-0.1.0
                                   rust-wasm-bindgen-0.2.93
                                   rust-wasm-bindgen-backend-0.2.93
                                   rust-wasm-bindgen-macro-0.2.93
                                   rust-wasm-bindgen-macro-support-0.2.93
                                   rust-wasm-bindgen-shared-0.2.93
                                   rust-watchman-client-0.9.0
                                   rust-web-sys-0.3.70
                                   rust-wezterm-bidi-0.2.3
                                   rust-wezterm-blob-leases-0.1.0
                                   rust-wezterm-color-types-0.3.0
                                   rust-wezterm-dynamic-0.2.1
                                   rust-wezterm-dynamic-derive-0.1.1
                                   rust-wezterm-input-types-0.1.0
                                   rust-whoami-1.6.0
                                   rust-winapi-0.3.9
                                   rust-winapi-i686-pc-windows-gnu-0.4.0
                                   rust-winapi-util-0.1.9
                                   rust-winapi-x86-64-pc-windows-gnu-0.4.0
                                   rust-windows-core-0.52.0
                                   rust-windows-link-0.1.0
                                   rust-windows-sys-0.48.0
                                   rust-windows-sys-0.52.0
                                   rust-windows-sys-0.59.0
                                   rust-windows-targets-0.48.5
                                   rust-windows-targets-0.52.6
                                   rust-windows-aarch64-gnullvm-0.48.5
                                   rust-windows-aarch64-gnullvm-0.52.6
                                   rust-windows-aarch64-msvc-0.48.5
                                   rust-windows-aarch64-msvc-0.52.6
                                   rust-windows-i686-gnu-0.48.5
                                   rust-windows-i686-gnu-0.52.6
                                   rust-windows-i686-gnullvm-0.52.6
                                   rust-windows-i686-msvc-0.48.5
                                   rust-windows-i686-msvc-0.52.6
                                   rust-windows-x86-64-gnu-0.48.5
                                   rust-windows-x86-64-gnu-0.52.6
                                   rust-windows-x86-64-gnullvm-0.48.5
                                   rust-windows-x86-64-gnullvm-0.52.6
                                   rust-windows-x86-64-msvc-0.48.5
                                   rust-windows-x86-64-msvc-0.52.6
                                   rust-winnow-0.7.4
                                   rust-winreg-0.52.0
                                   rust-wit-bindgen-rt-0.39.0
                                   rust-write16-1.0.0
                                   rust-writeable-0.5.5
                                   rust-yoke-0.7.5
                                   rust-yoke-derive-0.7.5
                                   rust-zerocopy-0.7.35
                                   rust-zerocopy-derive-0.7.35
                                   rust-zerofrom-0.1.6
                                   rust-zerofrom-derive-0.1.6
                                   rust-zerovec-0.10.4
                                   rust-zerovec-derive-0.10.3))
                     (keifu =>
                            (list rust-allocator-api2-0.2.21
                                  rust-android-system-properties-0.1.5
                                  rust-anstream-0.6.21
                                  rust-anstyle-1.0.13
                                  rust-anstyle-parse-0.2.7
                                  rust-anstyle-query-1.1.5
                                  rust-anstyle-wincon-3.0.11
                                  rust-anyhow-1.0.100
                                  rust-autocfg-1.5.0
                                  rust-bitflags-2.10.0
                                  rust-bumpalo-3.19.1
                                  rust-cassowary-0.3.0
                                  rust-castaway-0.2.4
                                  rust-cc-1.2.50
                                  rust-cfg-if-1.0.4
                                  rust-chrono-0.4.42
                                  rust-clap-4.5.53
                                  rust-clap-builder-4.5.53
                                  rust-clap-derive-4.5.49
                                  rust-clap-lex-0.7.6
                                  rust-colorchoice-1.0.4
                                  rust-compact-str-0.8.1
                                  rust-core-foundation-sys-0.8.7
                                  rust-crossterm-0.28.1
                                  rust-crossterm-winapi-0.9.1
                                  rust-darling-0.20.11
                                  rust-darling-core-0.20.11
                                  rust-darling-macro-0.20.11
                                  rust-dirs-5.0.1
                                  rust-dirs-sys-0.4.1
                                  rust-displaydoc-0.2.5
                                  rust-either-1.15.0
                                  rust-equivalent-1.0.2
                                  rust-errno-0.3.14
                                  rust-find-msvc-tools-0.1.5
                                  rust-fnv-1.0.7
                                  rust-foldhash-0.1.5
                                  rust-form-urlencoded-1.2.2
                                  rust-fuzzy-matcher-0.3.7
                                  rust-getrandom-0.2.17
                                  rust-getrandom-0.3.4
                                  rust-git2-0.19.0
                                  rust-hashbrown-0.15.5
                                  rust-hashbrown-0.16.1
                                  rust-heck-0.5.0
                                  rust-iana-time-zone-0.1.64
                                  rust-iana-time-zone-haiku-0.1.2
                                  rust-icu-collections-2.1.1
                                  rust-icu-locale-core-2.1.1
                                  rust-icu-normalizer-2.1.1
                                  rust-icu-normalizer-data-2.1.1
                                  rust-icu-properties-2.1.2
                                  rust-icu-properties-data-2.1.2
                                  rust-icu-provider-2.1.1
                                  rust-ident-case-1.0.1
                                  rust-idna-1.1.0
                                  rust-idna-adapter-1.2.1
                                  rust-indexmap-2.13.0
                                  rust-indoc-2.0.7
                                  rust-instability-0.3.10
                                  rust-is-terminal-polyfill-1.70.2
                                  rust-itertools-0.13.0
                                  rust-itoa-1.0.15
                                  rust-jobserver-0.1.34
                                  rust-js-sys-0.3.83
                                  rust-libc-0.2.178
                                  rust-libgit2-sys-0.17.0+1.8.1
                                  rust-libredox-0.1.12
                                  rust-libssh2-sys-0.3.1
                                  rust-libz-sys-1.1.23
                                  rust-linux-raw-sys-0.4.15
                                  rust-litemap-0.8.1
                                  rust-lock-api-0.4.14
                                  rust-log-0.4.29
                                  rust-lru-0.12.5
                                  rust-memchr-2.7.6
                                  rust-mio-1.1.1
                                  rust-num-traits-0.2.19
                                  rust-once-cell-1.21.3
                                  rust-once-cell-polyfill-1.70.2
                                  rust-openssl-probe-0.1.6
                                  rust-openssl-src-300.5.4+3.5.4
                                  rust-openssl-sys-0.9.111
                                  rust-option-ext-0.2.0
                                  rust-parking-lot-0.12.5
                                  rust-parking-lot-core-0.9.12
                                  rust-paste-1.0.15
                                  rust-percent-encoding-2.3.2
                                  rust-pkg-config-0.3.32
                                  rust-potential-utf-0.1.4
                                  rust-proc-macro2-1.0.103
                                  rust-quote-1.0.42
                                  rust-r-efi-5.3.0
                                  rust-ratatui-0.29.0
                                  rust-redox-syscall-0.5.18
                                  rust-redox-users-0.4.6
                                  rust-rustix-0.38.44
                                  rust-rustversion-1.0.22
                                  rust-ryu-1.0.20
                                  rust-scopeguard-1.2.0
                                  rust-serde-1.0.228
                                  rust-serde-core-1.0.228
                                  rust-serde-derive-1.0.228
                                  rust-serde-spanned-0.6.9
                                  rust-shlex-1.3.0
                                  rust-signal-hook-0.3.18
                                  rust-signal-hook-mio-0.2.5
                                  rust-signal-hook-registry-1.4.7
                                  rust-smallvec-1.15.1
                                  rust-stable-deref-trait-1.2.1
                                  rust-static-assertions-1.1.0
                                  rust-strsim-0.11.1
                                  rust-strum-0.26.3
                                  rust-strum-macros-0.26.4
                                  rust-syn-2.0.111
                                  rust-synstructure-0.13.2
                                  rust-thiserror-1.0.69
                                  rust-thiserror-2.0.17
                                  rust-thiserror-impl-1.0.69
                                  rust-thiserror-impl-2.0.17
                                  rust-thread-local-1.1.9
                                  rust-tinystr-0.8.2
                                  rust-toml-0.8.23
                                  rust-toml-datetime-0.6.11
                                  rust-toml-edit-0.22.27
                                  rust-toml-write-0.1.2
                                  rust-unicode-ident-1.0.22
                                  rust-unicode-segmentation-1.12.0
                                  rust-unicode-truncate-1.1.0
                                  rust-unicode-width-0.1.14
                                  rust-unicode-width-0.2.0
                                  rust-url-2.5.7
                                  rust-utf8-iter-1.0.4
                                  rust-utf8parse-0.2.2
                                  rust-vcpkg-0.2.15
                                  rust-wasi-0.11.1+wasi-snapshot-preview1
                                  rust-wasip2-1.0.1+wasi-0.2.4
                                  rust-wasm-bindgen-0.2.106
                                  rust-wasm-bindgen-macro-0.2.106
                                  rust-wasm-bindgen-macro-support-0.2.106
                                  rust-wasm-bindgen-shared-0.2.106
                                  rust-winapi-0.3.9
                                  rust-winapi-i686-pc-windows-gnu-0.4.0
                                  rust-winapi-x86-64-pc-windows-gnu-0.4.0
                                  rust-windows-core-0.62.2
                                  rust-windows-implement-0.60.2
                                  rust-windows-interface-0.59.3
                                  rust-windows-link-0.2.1
                                  rust-windows-result-0.4.1
                                  rust-windows-strings-0.5.1
                                  rust-windows-sys-0.48.0
                                  rust-windows-sys-0.59.0
                                  rust-windows-sys-0.61.2
                                  rust-windows-targets-0.48.5
                                  rust-windows-targets-0.52.6
                                  rust-windows-aarch64-gnullvm-0.48.5
                                  rust-windows-aarch64-gnullvm-0.52.6
                                  rust-windows-aarch64-msvc-0.48.5
                                  rust-windows-aarch64-msvc-0.52.6
                                  rust-windows-i686-gnu-0.48.5
                                  rust-windows-i686-gnu-0.52.6
                                  rust-windows-i686-gnullvm-0.52.6
                                  rust-windows-i686-msvc-0.48.5
                                  rust-windows-i686-msvc-0.52.6
                                  rust-windows-x86-64-gnu-0.48.5
                                  rust-windows-x86-64-gnu-0.52.6
                                  rust-windows-x86-64-gnullvm-0.48.5
                                  rust-windows-x86-64-gnullvm-0.52.6
                                  rust-windows-x86-64-msvc-0.48.5
                                  rust-windows-x86-64-msvc-0.52.6
                                  rust-winnow-0.7.14
                                  rust-wit-bindgen-0.46.0
                                  rust-writeable-0.6.2
                                  rust-yoke-0.8.1
                                  rust-yoke-derive-0.8.1
                                  rust-zerofrom-0.1.6
                                  rust-zerofrom-derive-0.1.6
                                  rust-zerotrie-0.2.3
                                  rust-zerovec-0.11.5
                                  rust-zerovec-derive-0.11.2))
                     (kooha =>
                            (list rust-aho-corasick-1.1.3
                                  rust-android-system-properties-0.1.5
                                  rust-android-tzdata-0.1.1
                                  rust-anyhow-1.0.99
                                  rust-arrayvec-0.7.6
                                  rust-async-channel-2.5.0
                                  rust-atomic-refcell-0.1.13
                                  rust-autocfg-1.5.0
                                  rust-bitflags-2.9.3
                                  rust-block-0.1.6
                                  rust-bumpalo-3.19.0
                                  rust-cairo-rs-0.21.1
                                  rust-cairo-sys-rs-0.21.1
                                  rust-cc-1.2.34
                                  rust-cfg-expr-0.20.2
                                  rust-cfg-if-1.0.3
                                  rust-chrono-0.4.41
                                  rust-color-quant-1.1.0
                                  rust-concurrent-queue-2.5.0
                                  rust-core-foundation-sys-0.8.7
                                  rust-crossbeam-utils-0.8.21
                                  rust-deluxe-0.5.0
                                  rust-deluxe-core-0.5.0
                                  rust-deluxe-macros-0.5.0
                                  rust-either-1.15.0
                                  rust-equivalent-1.0.2
                                  rust-event-listener-5.4.1
                                  rust-event-listener-strategy-0.5.4
                                  rust-field-offset-0.3.6
                                  rust-futures-channel-0.3.31
                                  rust-futures-core-0.3.31
                                  rust-futures-executor-0.3.31
                                  rust-futures-io-0.3.31
                                  rust-futures-macro-0.3.31
                                  rust-futures-task-0.3.31
                                  rust-futures-util-0.3.31
                                  rust-gdk4-0.10.0
                                  rust-gdk4-sys-0.10.0
                                  rust-gdk4-wayland-0.10.0
                                  rust-gdk4-wayland-sys-0.10.0
                                  rust-gdk4-win32-0.10.0
                                  rust-gdk4-win32-sys-0.10.0
                                  rust-gdk4-x11-0.10.0
                                  rust-gdk4-x11-sys-0.10.0
                                  rust-gdk-pixbuf-0.21.1
                                  rust-gdk-pixbuf-sys-0.21.1
                                  rust-gettext-rs-0.7.2
                                  rust-gettext-sys-0.22.5
                                  rust-gif-0.13.3
                                  rust-gio-0.21.1
                                  rust-gio-sys-0.21.1
                                  rust-glib-0.21.1
                                  rust-glib-macros-0.21.0
                                  rust-glib-sys-0.21.1
                                  rust-gobject-sys-0.21.1
                                  rust-graphene-rs-0.21.1
                                  rust-graphene-sys-0.21.1
                                  rust-gsettings-macro-0.2.2
                                  rust-gsk4-0.10.0
                                  rust-gsk4-sys-0.10.0
                                  rust-gst-plugin-gif-0.14.0
                                  rust-gst-plugin-gtk4-0.14.0
                                  rust-gst-plugin-version-helper-0.8.3
                                  rust-gstreamer-0.24.1
                                  rust-gstreamer-allocators-0.24.0
                                  rust-gstreamer-allocators-sys-0.24.0
                                  rust-gstreamer-base-0.24.0
                                  rust-gstreamer-base-sys-0.24.0
                                  rust-gstreamer-gl-0.24.1
                                  rust-gstreamer-gl-egl-0.24.0
                                  rust-gstreamer-gl-egl-sys-0.24.0
                                  rust-gstreamer-gl-sys-0.24.1
                                  rust-gstreamer-gl-wayland-0.24.0
                                  rust-gstreamer-gl-wayland-sys-0.24.0
                                  rust-gstreamer-gl-x11-0.24.0
                                  rust-gstreamer-gl-x11-sys-0.24.0
                                  rust-gstreamer-sys-0.24.0
                                  rust-gstreamer-video-0.24.1
                                  rust-gstreamer-video-sys-0.24.1
                                  rust-gtk4-0.10.0
                                  rust-gtk4-macros-0.10.0
                                  rust-gtk4-sys-0.10.0
                                  rust-hashbrown-0.15.5
                                  rust-heck-0.4.1
                                  rust-heck-0.5.0
                                  rust-iana-time-zone-0.1.63
                                  rust-iana-time-zone-haiku-0.1.2
                                  rust-if-chain-1.0.2
                                  rust-indexmap-2.11.0
                                  rust-itertools-0.14.0
                                  rust-itoa-1.0.15
                                  rust-js-sys-0.3.77
                                  rust-kstring-2.0.2
                                  rust-lazy-static-1.5.0
                                  rust-libadwaita-0.8.0
                                  rust-libadwaita-sys-0.8.0
                                  rust-libc-0.2.175
                                  rust-locale-config-0.3.0
                                  rust-log-0.4.27
                                  rust-malloc-buf-0.0.6
                                  rust-memchr-2.7.5
                                  rust-memoffset-0.9.1
                                  rust-muldiv-1.0.1
                                  rust-nu-ansi-term-0.46.0
                                  rust-num-integer-0.1.46
                                  rust-num-rational-0.4.2
                                  rust-num-traits-0.2.19
                                  rust-objc-0.2.7
                                  rust-objc-foundation-0.1.1
                                  rust-objc-id-0.1.1
                                  rust-once-cell-1.21.3
                                  rust-option-operations-0.5.0
                                  rust-overload-0.1.1
                                  rust-pango-0.21.1
                                  rust-pango-sys-0.21.1
                                  rust-parking-2.2.1
                                  rust-paste-1.0.15
                                  rust-pin-project-lite-0.2.16
                                  rust-pin-utils-0.1.0
                                  rust-pkg-config-0.3.32
                                  rust-proc-macro2-1.0.101
                                  rust-proc-macro-crate-1.3.1
                                  rust-proc-macro-crate-3.3.0
                                  rust-proc-macro-error-1.0.4
                                  rust-proc-macro-error-attr-1.0.4
                                  rust-quick-xml-0.37.5
                                  rust-quote-1.0.40
                                  rust-regex-1.11.1
                                  rust-regex-automata-0.4.9
                                  rust-regex-syntax-0.8.5
                                  rust-rustc-version-0.4.1
                                  rust-rustversion-1.0.22
                                  rust-ryu-1.0.20
                                  rust-semver-1.0.26
                                  rust-serde-1.0.219
                                  rust-serde-derive-1.0.219
                                  rust-serde-spanned-0.6.9
                                  rust-serde-yaml-0.9.34+deprecated
                                  rust-sharded-slab-0.1.7
                                  rust-shlex-1.3.0
                                  rust-slab-0.4.11
                                  rust-smallvec-1.15.1
                                  rust-static-assertions-1.1.0
                                  rust-strsim-0.10.0
                                  rust-syn-1.0.109
                                  rust-syn-2.0.106
                                  rust-system-deps-7.0.5
                                  rust-target-lexicon-0.13.2
                                  rust-temp-dir-0.1.16
                                  rust-thiserror-2.0.16
                                  rust-thiserror-impl-2.0.16
                                  rust-thread-local-1.1.9
                                  rust-toml-0.8.23
                                  rust-toml-datetime-0.6.11
                                  rust-toml-datetime-0.7.0
                                  rust-toml-edit-0.19.15
                                  rust-toml-edit-0.22.27
                                  rust-toml-edit-0.23.4
                                  rust-toml-parser-1.0.2
                                  rust-tracing-0.1.41
                                  rust-tracing-attributes-0.1.30
                                  rust-tracing-core-0.1.34
                                  rust-tracing-log-0.2.0
                                  rust-tracing-subscriber-0.3.19
                                  rust-unicode-ident-1.0.18
                                  rust-unsafe-libyaml-0.2.11
                                  rust-valuable-0.1.1
                                  rust-version-check-0.9.5
                                  rust-version-compare-0.2.0
                                  rust-wasm-bindgen-0.2.100
                                  rust-wasm-bindgen-backend-0.2.100
                                  rust-wasm-bindgen-macro-0.2.100
                                  rust-wasm-bindgen-macro-support-0.2.100
                                  rust-wasm-bindgen-shared-0.2.100
                                  rust-weezl-0.1.10
                                  rust-winapi-0.3.9
                                  rust-winapi-i686-pc-windows-gnu-0.4.0
                                  rust-winapi-x86-64-pc-windows-gnu-0.4.0
                                  rust-windows-aarch64-gnullvm-0.53.0
                                  rust-windows-aarch64-msvc-0.53.0
                                  rust-windows-core-0.61.2
                                  rust-windows-i686-gnu-0.53.0
                                  rust-windows-i686-gnullvm-0.53.0
                                  rust-windows-i686-msvc-0.53.0
                                  rust-windows-implement-0.60.0
                                  rust-windows-interface-0.59.1
                                  rust-windows-link-0.1.3
                                  rust-windows-result-0.3.4
                                  rust-windows-strings-0.4.2
                                  rust-windows-sys-0.60.2
                                  rust-windows-targets-0.53.3
                                  rust-windows-x86-64-gnu-0.53.0
                                  rust-windows-x86-64-gnullvm-0.53.0
                                  rust-windows-x86-64-msvc-0.53.0
                                  rust-winnow-0.5.40
                                  rust-winnow-0.7.13))
                     (oha =>
                          (list rust-actix-codec-0.5.2
                           rust-actix-http-3.11.2
                           rust-actix-macros-0.2.4
                           rust-actix-router-0.5.3
                           rust-actix-rt-2.11.0
                           rust-actix-server-2.6.0
                           rust-actix-service-2.0.3
                           rust-actix-utils-3.0.1
                           rust-actix-web-4.12.1
                           rust-actix-web-codegen-4.3.0
                           rust-adler2-2.0.1
                           rust-ahash-0.7.8
                           rust-ahash-0.8.12
                           rust-aho-corasick-1.1.4
                           rust-alloc-no-stdlib-2.0.4
                           rust-alloc-stdlib-0.2.2
                           rust-allocator-api2-0.2.21
                           rust-android-system-properties-0.1.5
                           rust-anstream-0.6.21
                           rust-anstyle-1.0.13
                           rust-anstyle-parse-0.2.7
                           rust-anstyle-query-1.1.5
                           rust-anstyle-wincon-3.0.11
                           rust-anyhow-1.0.100
                           rust-arc-swap-1.8.0
                           rust-arrayvec-0.7.6
                           rust-asn1-rs-0.7.1
                           rust-asn1-rs-derive-0.6.0
                           rust-asn1-rs-impl-0.2.0
                           rust-async-trait-0.1.89
                           rust-atomic-waker-1.1.2
                           rust-autocfg-1.5.0
                           rust-average-0.16.0
                           rust-aws-lc-rs-1.15.3
                           rust-aws-lc-sys-0.36.0
                           rust-aws-sign-v4-0.3.0
                           rust-axum-0.8.8
                           rust-axum-core-0.5.6
                           rust-axum-server-0.8.0
                           rust-base64-0.22.1
                           rust-bit-set-0.8.0
                           rust-bit-vec-0.8.0
                           rust-bitflags-2.10.0
                           rust-bitvec-1.0.1
                           rust-block-buffer-0.10.4
                           rust-borrow-or-share-0.2.4
                           rust-borsh-1.6.0
                           rust-borsh-derive-1.6.0
                           rust-brotli-8.0.2
                           rust-brotli-decompressor-5.0.0
                           rust-bumpalo-3.19.1
                           rust-byte-unit-5.2.0
                           rust-bytecheck-0.6.12
                           rust-bytecheck-derive-0.6.12
                           rust-bytecount-0.6.9
                           rust-bytes-1.11.0
                           rust-bytestring-1.5.0
                           rust-cassowary-0.3.0
                           rust-castaway-0.2.4
                           rust-cc-1.2.53
                           rust-cesu8-1.1.0
                           rust-cfg-if-1.0.4
                           rust-cfg-aliases-0.2.1
                           rust-chrono-0.4.43
                           rust-clap-4.5.54
                           rust-clap-cargo-0.18.3
                           rust-clap-builder-4.5.54
                           rust-clap-derive-4.5.49
                           rust-clap-lex-0.7.7
                           rust-cmake-0.1.57
                           rust-colorchoice-1.0.4
                           rust-combine-4.6.7
                           rust-compact-str-0.8.1
                           rust-convert-case-0.10.0
                           rust-cookie-0.16.2
                           rust-core-foundation-0.9.4
                           rust-core-foundation-0.10.1
                           rust-core-foundation-sys-0.8.7
                           rust-cpufeatures-0.2.17
                           rust-crc32fast-1.5.0
                           rust-critical-section-1.2.0
                           rust-crossbeam-channel-0.5.15
                           rust-crossbeam-epoch-0.9.18
                           rust-crossbeam-utils-0.8.21
                           rust-crossterm-0.28.1
                           rust-crossterm-winapi-0.9.1
                           rust-crypto-common-0.1.7
                           rust-ctor-0.6.3
                           rust-ctor-proc-macro-0.0.7
                           rust-darling-0.20.11
                           rust-darling-core-0.20.11
                           rust-darling-macro-0.20.11
                           rust-data-encoding-2.10.0
                           rust-der-parser-10.0.0
                           rust-deranged-0.5.5
                           rust-derive-more-2.1.1
                           rust-derive-more-impl-2.1.1
                           rust-difflib-0.4.0
                           rust-digest-0.10.7
                           rust-displaydoc-0.2.5
                           rust-dtor-0.1.1
                           rust-dtor-proc-macro-0.0.6
                           rust-dunce-1.0.5
                           rust-dyn-clone-1.0.20
                           rust-easy-cast-0.5.4
                           rust-either-1.15.0
                           rust-email-address-0.2.9
                           rust-encoding-rs-0.8.35
                           rust-enum-as-inner-0.6.1
                           rust-equivalent-1.0.2
                           rust-errno-0.3.14
                           rust-fallible-iterator-0.3.0
                           rust-fallible-streaming-iterator-0.1.9
                           rust-fancy-regex-0.16.2
                           rust-fastbloom-0.14.1
                           rust-fastrand-2.3.0
                           rust-find-msvc-tools-0.1.8
                           rust-flate2-1.1.8
                           rust-float-cmp-0.10.0
                           rust-float-ord-0.3.2
                           rust-fluent-uri-0.4.1
                           rust-fnv-1.0.7
                           rust-foldhash-0.1.5
                           rust-foldhash-0.2.0
                           rust-foreign-types-0.3.2
                           rust-foreign-types-shared-0.1.1
                           rust-form-urlencoded-1.2.2
                           rust-fraction-0.15.3
                           rust-fs-err-3.2.2
                           rust-fs-extra-1.3.0
                           rust-funty-2.0.0
                           rust-futures-0.3.31
                           rust-futures-channel-0.3.31
                           rust-futures-core-0.3.31
                           rust-futures-executor-0.3.31
                           rust-futures-io-0.3.31
                           rust-futures-macro-0.3.31
                           rust-futures-sink-0.3.31
                           rust-futures-task-0.3.31
                           rust-futures-timer-3.0.3
                           rust-futures-util-0.3.31
                           rust-generic-array-0.14.7
                           rust-getrandom-0.2.17
                           rust-getrandom-0.3.4
                           rust-glob-0.3.3
                           rust-h2-0.3.27
                           rust-h2-0.4.13
                           rust-h3-0.0.8
                           rust-h3-quinn-0.0.10
                           rust-hashbrown-0.12.3
                           rust-hashbrown-0.15.5
                           rust-hashbrown-0.16.1
                           rust-hashlink-0.10.0
                           rust-heck-0.5.0
                           rust-hermit-abi-0.5.2
                           rust-hex-0.4.3
                           rust-hickory-proto-0.25.2
                           rust-hickory-resolver-0.25.2
                           rust-http-0.2.12
                           rust-http-1.4.0
                           rust-http-body-1.0.1
                           rust-http-body-util-0.1.3
                           rust-http-mitm-proxy-0.16.0
                           rust-httparse-1.10.1
                           rust-httpdate-1.0.3
                           rust-humantime-2.3.0
                           rust-hyper-1.8.1
                           rust-hyper-util-0.1.19
                           rust-iana-time-zone-0.1.64
                           rust-iana-time-zone-haiku-0.1.2
                           rust-icu-collections-2.1.1
                           rust-icu-locale-core-2.1.1
                           rust-icu-normalizer-2.1.1
                           rust-icu-normalizer-data-2.1.1
                           rust-icu-properties-2.1.2
                           rust-icu-properties-data-2.1.2
                           rust-icu-provider-2.1.1
                           rust-ident-case-1.0.1
                           rust-idna-1.1.0
                           rust-idna-adapter-1.2.1
                           rust-impl-more-0.1.9
                           rust-indexmap-2.13.0
                           rust-indoc-2.0.7
                           rust-instability-0.3.10
                           rust-ipconfig-0.3.2
                           rust-ipnet-2.11.0
                           rust-iri-string-0.7.10
                           rust-is-terminal-polyfill-1.70.2
                           rust-itertools-0.13.0
                           rust-itoa-1.0.17
                           rust-jni-0.21.1
                           rust-jni-sys-0.3.0
                           rust-jobserver-0.1.34
                           rust-js-sys-0.3.85
                           rust-jsonschema-0.37.4
                           rust-kanal-0.1.1
                           rust-language-tags-0.3.2
                           rust-lazy-static-1.5.0
                           rust-libc-0.2.180
                           rust-libm-0.2.15
                           rust-libsqlite3-sys-0.35.0
                           rust-linux-raw-sys-0.4.15
                           rust-linux-raw-sys-0.11.0
                           rust-litemap-0.8.1
                           rust-local-channel-0.1.5
                           rust-local-waker-0.1.4
                           rust-lock-api-0.4.14
                           rust-log-0.4.29
                           rust-lru-0.12.5
                           rust-lru-slab-0.1.2
                           rust-matchit-0.8.4
                           rust-memchr-2.7.6
                           rust-memoffset-0.9.1
                           rust-mime-0.3.17
                           rust-minimal-lexical-0.2.1
                           rust-miniz-oxide-0.8.9
                           rust-mio-1.1.1
                           rust-moka-0.12.12
                           rust-native-tls-0.2.14
                           rust-nix-0.30.1
                           rust-nom-7.1.3
                           rust-normalize-line-endings-0.3.0
                           rust-num-0.4.3
                           rust-num-bigint-0.4.6
                           rust-num-cmp-0.1.0
                           rust-num-complex-0.4.6
                           rust-num-conv-0.1.0
                           rust-num-integer-0.1.46
                           rust-num-iter-0.1.45
                           rust-num-rational-0.4.2
                           rust-num-traits-0.2.19
                           rust-num-cpus-1.17.0
                           rust-oid-registry-0.8.1
                           rust-once-cell-1.21.3
                           rust-once-cell-polyfill-1.70.2
                           rust-openssl-0.10.75
                           rust-openssl-macros-0.1.1
                           rust-openssl-probe-0.1.6
                           rust-openssl-probe-0.2.0
                           rust-openssl-sys-0.9.111
                           rust-outref-0.5.2
                           rust-parking-lot-0.12.5
                           rust-parking-lot-core-0.9.12
                           rust-paste-1.0.15
                           rust-pem-3.0.6
                           rust-percent-encoding-2.3.2
                           rust-pin-project-lite-0.2.16
                           rust-pin-utils-0.1.0
                           rust-pkg-config-0.3.32
                           rust-portable-atomic-1.13.0
                           rust-potential-utf-0.1.4
                           rust-powerfmt-0.2.0
                           rust-ppv-lite86-0.2.21
                           rust-predicates-3.1.3
                           rust-predicates-core-1.0.9
                           rust-proc-macro-crate-3.4.0
                           rust-proc-macro2-1.0.105
                           rust-ptr-meta-0.1.4
                           rust-ptr-meta-derive-0.1.4
                           rust-quinn-0.11.9
                           rust-quinn-proto-0.11.13
                           rust-quinn-udp-0.5.14
                           rust-quote-1.0.43
                           rust-r-efi-5.3.0
                           rust-radium-0.7.0
                           rust-rand-0.8.5
                           rust-rand-0.9.2
                           rust-rand-chacha-0.3.1
                           rust-rand-chacha-0.9.0
                           rust-rand-core-0.6.4
                           rust-rand-core-0.9.5
                           rust-rand-regex-0.18.1
                           rust-ratatui-0.29.0
                           rust-rcgen-0.14.6
                           rust-redox-syscall-0.5.18
                           rust-ref-cast-1.0.25
                           rust-ref-cast-impl-1.0.25
                           rust-referencing-0.37.4
                           rust-regex-1.12.2
                           rust-regex-automata-0.4.13
                           rust-regex-lite-0.1.8
                           rust-regex-syntax-0.8.8
                           rust-relative-path-1.9.3
                           rust-rend-0.4.2
                           rust-reqwest-0.12.28
                           rust-resolv-conf-0.7.6
                           rust-ring-0.17.14
                           rust-rkyv-0.7.46
                           rust-rkyv-derive-0.7.46
                           rust-rlimit-0.10.2
                           rust-rstest-0.26.1
                           rust-rstest-macros-0.26.1
                           rust-rstest-reuse-0.7.0
                           rust-rusqlite-0.37.0
                           rust-rust-decimal-1.40.0
                           rust-rustc-hash-2.1.1
                           rust-rustc-version-0.4.1
                           rust-rusticata-macros-4.1.0
                           rust-rustix-0.38.44
                           rust-rustix-1.1.3
                           rust-rustls-0.23.36
                           rust-rustls-native-certs-0.8.3
                           rust-rustls-pki-types-1.14.0
                           rust-rustls-platform-verifier-0.6.2
                           rust-rustls-platform-verifier-android-0.1.1
                           rust-rustls-webpki-0.103.9
                           rust-rustversion-1.0.22
                           rust-ryu-1.0.22
                           rust-same-file-1.0.6
                           rust-schannel-0.1.28
                           rust-schemars-1.2.0
                           rust-scopeguard-1.2.0
                           rust-seahash-4.1.0
                           rust-security-framework-2.11.1
                           rust-security-framework-3.5.1
                           rust-security-framework-sys-2.15.0
                           rust-semver-1.0.27
                           rust-serde-1.0.228
                           rust-serde-core-1.0.228
                           rust-serde-derive-1.0.228
                           rust-serde-json-1.0.149
                           rust-serde-path-to-error-0.1.20
                           rust-serde-urlencoded-0.7.1
                           rust-sha1-0.10.6
                           rust-sha2-0.10.9
                           rust-sha256-1.6.0
                           rust-shlex-1.3.0
                           rust-signal-hook-0.3.18
                           rust-signal-hook-mio-0.2.5
                           rust-signal-hook-registry-1.4.8
                           rust-simd-adler32-0.3.8
                           rust-simdutf8-0.1.5
                           rust-siphasher-1.0.1
                           rust-slab-0.4.11
                           rust-smallvec-1.15.1
                           rust-socket2-0.5.10
                           rust-socket2-0.6.1
                           rust-stable-deref-trait-1.2.1
                           rust-static-assertions-1.1.0
                           rust-strsim-0.11.1
                           rust-strum-0.26.3
                           rust-strum-macros-0.26.4
                           rust-subtle-2.6.1
                           rust-syn-1.0.109
                           rust-syn-2.0.114
                           rust-sync-wrapper-1.0.2
                           rust-synstructure-0.13.2
                           rust-tagptr-0.2.0
                           rust-tap-1.0.1
                           rust-tempfile-3.24.0
                           rust-thiserror-1.0.69
                           rust-thiserror-2.0.17
                           rust-thiserror-impl-1.0.69
                           rust-thiserror-impl-2.0.17
                           rust-tikv-jemalloc-sys-0.6.1+5.3.0-1-ge13ca993e8ccb9ba9847cc330696e02839f328f7
                           rust-tikv-jemallocator-0.6.1
                           rust-time-0.3.45
                           rust-time-core-0.1.7
                           rust-time-macros-0.2.25
                           rust-tinystr-0.8.2
                           rust-tinyvec-1.10.0
                           rust-tinyvec-macros-0.1.1
                           rust-tokio-1.49.0
                           rust-tokio-macros-2.6.0
                           rust-tokio-native-tls-0.3.1
                           rust-tokio-rustls-0.26.4
                           rust-tokio-util-0.7.18
                           rust-tokio-vsock-0.7.2
                           rust-toml-datetime-0.7.5+spec-1.1.0
                           rust-toml-edit-0.23.10+spec-1.0.0
                           rust-toml-parser-1.0.6+spec-1.1.0
                           rust-tower-0.5.3
                           rust-tower-http-0.6.8
                           rust-tower-layer-0.3.3
                           rust-tower-service-0.3.3
                           rust-tracing-0.1.44
                           rust-tracing-attributes-0.1.31
                           rust-tracing-core-0.1.36
                           rust-try-lock-0.2.5
                           rust-typenum-1.19.0
                           rust-unicode-general-category-1.1.0
                           rust-unicode-ident-1.0.22
                           rust-unicode-segmentation-1.12.0
                           rust-unicode-truncate-1.1.0
                           rust-unicode-width-0.1.14
                           rust-unicode-width-0.2.0
                           rust-unicode-xid-0.2.6
                           rust-untrusted-0.7.1
                           rust-untrusted-0.9.0
                           rust-url-2.5.8
                           rust-utf8-width-0.1.8
                           rust-utf8-iter-1.0.4
                           rust-utf8parse-0.2.2
                           rust-uuid-1.19.0
                           rust-uuid-simd-0.8.0
                           rust-vcpkg-0.2.15
                           rust-version-check-0.9.5
                           rust-vsimd-0.8.0
                           rust-vsock-0.5.2
                           rust-walkdir-2.5.0
                           rust-want-0.3.1
                           rust-wasi-0.11.1+wasi-snapshot-preview1
                           rust-wasip2-1.0.1+wasi-0.2.4
                           rust-wasm-bindgen-0.2.108
                           rust-wasm-bindgen-futures-0.4.58
                           rust-wasm-bindgen-macro-0.2.108
                           rust-wasm-bindgen-macro-support-0.2.108
                           rust-wasm-bindgen-shared-0.2.108
                           rust-web-sys-0.3.85
                           rust-web-time-1.1.0
                           rust-webpki-root-certs-1.0.5
                           rust-widestring-1.2.1
                           rust-winapi-0.3.9
                           rust-winapi-i686-pc-windows-gnu-0.4.0
                           rust-winapi-util-0.1.11
                           rust-winapi-x86-64-pc-windows-gnu-0.4.0
                           rust-windows-core-0.62.2
                           rust-windows-implement-0.60.2
                           rust-windows-interface-0.59.3
                           rust-windows-link-0.2.1
                           rust-windows-result-0.4.1
                           rust-windows-strings-0.5.1
                           rust-windows-sys-0.45.0
                           rust-windows-sys-0.48.0
                           rust-windows-sys-0.52.0
                           rust-windows-sys-0.59.0
                           rust-windows-sys-0.60.2
                           rust-windows-sys-0.61.2
                           rust-windows-targets-0.42.2
                           rust-windows-targets-0.48.5
                           rust-windows-targets-0.52.6
                           rust-windows-targets-0.53.5
                           rust-windows-aarch64-gnullvm-0.42.2
                           rust-windows-aarch64-gnullvm-0.48.5
                           rust-windows-aarch64-gnullvm-0.52.6
                           rust-windows-aarch64-gnullvm-0.53.1
                           rust-windows-aarch64-msvc-0.42.2
                           rust-windows-aarch64-msvc-0.48.5
                           rust-windows-aarch64-msvc-0.52.6
                           rust-windows-aarch64-msvc-0.53.1
                           rust-windows-i686-gnu-0.42.2
                           rust-windows-i686-gnu-0.48.5
                           rust-windows-i686-gnu-0.52.6
                           rust-windows-i686-gnu-0.53.1
                           rust-windows-i686-gnullvm-0.52.6
                           rust-windows-i686-gnullvm-0.53.1
                           rust-windows-i686-msvc-0.42.2
                           rust-windows-i686-msvc-0.48.5
                           rust-windows-i686-msvc-0.52.6
                           rust-windows-i686-msvc-0.53.1
                           rust-windows-x86-64-gnu-0.42.2
                           rust-windows-x86-64-gnu-0.48.5
                           rust-windows-x86-64-gnu-0.52.6
                           rust-windows-x86-64-gnu-0.53.1
                           rust-windows-x86-64-gnullvm-0.42.2
                           rust-windows-x86-64-gnullvm-0.48.5
                           rust-windows-x86-64-gnullvm-0.52.6
                           rust-windows-x86-64-gnullvm-0.53.1
                           rust-windows-x86-64-msvc-0.42.2
                           rust-windows-x86-64-msvc-0.48.5
                           rust-windows-x86-64-msvc-0.52.6
                           rust-windows-x86-64-msvc-0.53.1
                           rust-winnow-0.7.14
                           rust-winreg-0.50.0
                           rust-wit-bindgen-0.46.0
                           rust-writeable-0.6.2
                           rust-wyz-0.5.1
                           rust-x509-parser-0.18.0
                           rust-yasna-0.5.2
                           rust-yoke-0.8.1
                           rust-yoke-derive-0.8.1
                           rust-zerocopy-0.8.33
                           rust-zerocopy-derive-0.8.33
                           rust-zerofrom-0.1.6
                           rust-zerofrom-derive-0.1.6
                           rust-zeroize-1.8.2
                           rust-zerotrie-0.2.3
                           rust-zerovec-0.11.5
                           rust-zerovec-derive-0.11.2
                           rust-zmij-1.0.15
                           rust-zstd-0.13.3
                           rust-zstd-safe-7.2.4
                           rust-zstd-sys-2.0.16+zstd.1.5.7))
                     (overskride =>
                                 (list rust-addr2line-0.21.0
                                  rust-adler-1.0.2
                                  rust-aho-corasick-1.1.2
                                  rust-anyhow-1.0.79
                                  rust-autocfg-1.1.0
                                  rust-backtrace-0.3.69
                                  rust-bitflags-1.3.2
                                  rust-bitflags-2.4.2
                                  rust-block-0.1.6
                                  rust-bluer-0.16.1
                                  rust-bytes-1.5.0
                                  rust-cairo-rs-0.18.5
                                  rust-cairo-sys-rs-0.18.2
                                  rust-cc-1.0.83
                                  rust-cfg-expr-0.15.7
                                  rust-cfg-if-1.0.0
                                  rust-custom-debug-0.5.1
                                  rust-custom-debug-derive-0.5.1
                                  rust-dbus-0.9.7
                                  rust-dbus-crossroads-0.5.2
                                  rust-dbus-tokio-0.7.6
                                  rust-displaydoc-0.2.4
                                  rust-equivalent-1.0.1
                                  rust-field-offset-0.3.6
                                  rust-futures-0.3.30
                                  rust-futures-channel-0.3.30
                                  rust-futures-core-0.3.30
                                  rust-futures-executor-0.3.30
                                  rust-futures-io-0.3.30
                                  rust-futures-macro-0.3.30
                                  rust-futures-sink-0.3.30
                                  rust-futures-task-0.3.30
                                  rust-futures-util-0.3.30
                                  rust-gdk-pixbuf-0.18.5
                                  rust-gdk-pixbuf-sys-0.18.0
                                  rust-gdk4-0.7.3
                                  rust-gdk4-sys-0.7.2
                                  rust-getrandom-0.2.12
                                  rust-gettext-rs-0.7.0
                                  rust-gettext-sys-0.21.3
                                  rust-gimli-0.28.1
                                  rust-gio-0.18.4
                                  rust-gio-sys-0.18.1
                                  rust-glib-0.18.5
                                  rust-glib-macros-0.18.5
                                  rust-glib-sys-0.18.1
                                  rust-gobject-sys-0.18.0
                                  rust-graphene-rs-0.18.1
                                  rust-graphene-sys-0.18.1
                                  rust-gsk4-0.7.3
                                  rust-gsk4-sys-0.7.3
                                  rust-gtk4-0.7.3
                                  rust-gtk4-macros-0.7.2
                                  rust-gtk4-sys-0.7.3
                                  rust-hashbrown-0.14.3
                                  rust-heck-0.4.1
                                  rust-hermit-abi-0.3.5
                                  rust-hex-0.4.3
                                  rust-indexmap-2.2.2
                                  rust-itoa-1.0.10
                                  rust-lazy-static-1.4.0
                                  rust-libadwaita-0.5.3
                                  rust-libadwaita-sys-0.5.3
                                  rust-libc-0.2.153
                                  rust-libdbus-sys-0.2.5
                                  rust-libpulse-binding-2.28.1
                                  rust-libpulse-sys-1.21.0
                                  rust-locale-config-0.3.0
                                  rust-lock-api-0.4.11
                                  rust-log-0.4.20
                                  rust-macaddr-1.0.1
                                  rust-malloc-buf-0.0.6
                                  rust-memchr-2.7.1
                                  rust-memoffset-0.9.0
                                  rust-miniz-oxide-0.7.2
                                  rust-mio-0.8.10
                                  rust-nix-0.27.1
                                  rust-num-derive-0.3.3
                                  rust-num-derive-0.4.2
                                  rust-num-traits-0.2.18
                                  rust-num-cpus-1.16.0
                                  rust-objc-0.2.7
                                  rust-objc-foundation-0.1.1
                                  rust-objc-id-0.1.1
                                  rust-object-0.32.2
                                  rust-once-cell-1.19.0
                                  rust-pango-0.18.3
                                  rust-pango-sys-0.18.0
                                  rust-parking-lot-0.12.1
                                  rust-parking-lot-core-0.9.9
                                  rust-phf-0.11.2
                                  rust-phf-generator-0.11.2
                                  rust-phf-macros-0.11.2
                                  rust-phf-shared-0.11.2
                                  rust-pin-project-1.1.4
                                  rust-pin-project-internal-1.1.4
                                  rust-pin-project-lite-0.2.13
                                  rust-pin-utils-0.1.0
                                  rust-pkg-config-0.3.29
                                  rust-proc-macro-crate-1.3.1
                                  rust-proc-macro-crate-2.0.2
                                  rust-proc-macro-error-1.0.4
                                  rust-proc-macro-error-attr-1.0.4
                                  rust-proc-macro2-1.0.78
                                  rust-quote-1.0.35
                                  rust-rand-0.8.5
                                  rust-rand-core-0.6.4
                                  rust-redox-syscall-0.4.1
                                  rust-regex-1.10.3
                                  rust-regex-automata-0.4.5
                                  rust-regex-syntax-0.8.2
                                  rust-rustc-demangle-0.1.23
                                  rust-rustc-version-0.4.0
                                  rust-rustversion-1.0.14
                                  rust-ryu-1.0.16
                                  rust-scopeguard-1.2.0
                                  rust-semver-1.0.21
                                  rust-serde-1.0.196
                                  rust-serde-derive-1.0.196
                                  rust-serde-json-1.0.113
                                  rust-serde-spanned-0.6.5
                                  rust-signal-hook-registry-1.4.1
                                  rust-siphasher-0.3.11
                                  rust-slab-0.4.9
                                  rust-smallvec-1.13.1
                                  rust-socket2-0.5.5
                                  rust-strum-0.25.0
                                  rust-strum-macros-0.25.3
                                  rust-syn-1.0.109
                                  rust-syn-2.0.48
                                  rust-synstructure-0.12.6
                                  rust-system-deps-6.2.0
                                  rust-target-lexicon-0.12.13
                                  rust-temp-dir-0.1.12
                                  rust-thiserror-1.0.56
                                  rust-thiserror-impl-1.0.56
                                  rust-tokio-1.36.0
                                  rust-tokio-macros-2.2.0
                                  rust-tokio-stream-0.1.14
                                  rust-tokio-util-0.7.10
                                  rust-toml-0.8.2
                                  rust-toml-datetime-0.6.3
                                  rust-toml-edit-0.19.15
                                  rust-toml-edit-0.20.2
                                  rust-unicode-ident-1.0.12
                                  rust-unicode-xid-0.2.4
                                  rust-uuid-1.7.0
                                  rust-version-compare-0.1.1
                                  rust-version-check-0.9.4
                                  rust-wasi-0.11.0+wasi-snapshot-preview1
                                  rust-winapi-0.3.9
                                  rust-winapi-i686-pc-windows-gnu-0.4.0
                                  rust-winapi-x86-64-pc-windows-gnu-0.4.0
                                  rust-windows-sys-0.48.0
                                  rust-windows-targets-0.48.5
                                  rust-windows-aarch64-gnullvm-0.48.5
                                  rust-windows-aarch64-msvc-0.48.5
                                  rust-windows-i686-gnu-0.48.5
                                  rust-windows-i686-msvc-0.48.5
                                  rust-windows-x86-64-gnu-0.48.5
                                  rust-windows-x86-64-gnullvm-0.48.5
                                  rust-windows-x86-64-msvc-0.48.5
                                  rust-winnow-0.5.39))
                     (package-version-server =>
                                             (list rust-addr2line-0.22.0
                                              rust-adler-1.0.2
                                              rust-aho-corasick-1.1.3
                                              rust-android-tzdata-0.1.1
                                              rust-android-system-properties-0.1.5
                                              rust-anyhow-1.0.92
                                              rust-async-trait-0.1.80
                                              rust-auto-impl-1.2.0
                                              rust-autocfg-1.3.0
                                              rust-backtrace-0.3.73
                                              rust-base64-0.21.7
                                              rust-bitflags-1.3.2
                                              rust-bitflags-2.5.0
                                              rust-bumpalo-3.16.0
                                              rust-bytes-1.6.0
                                              rust-cc-1.0.99
                                              rust-cfg-if-1.0.0
                                              rust-chrono-0.4.38
                                              rust-chrono-humanize-0.2.3
                                              rust-core-foundation-0.9.4
                                              rust-core-foundation-sys-0.8.6
                                              rust-dashmap-5.5.3
                                              rust-deranged-0.3.11
                                              rust-either-1.13.0
                                              rust-encoding-rs-0.8.34
                                              rust-equivalent-1.0.1
                                              rust-errno-0.3.9
                                              rust-fastrand-2.1.0
                                              rust-fnv-1.0.7
                                              rust-foreign-types-0.3.2
                                              rust-foreign-types-shared-0.1.1
                                              rust-form-urlencoded-1.2.1
                                              rust-futures-0.3.30
                                              rust-futures-channel-0.3.30
                                              rust-futures-core-0.3.30
                                              rust-futures-io-0.3.30
                                              rust-futures-macro-0.3.30
                                              rust-futures-sink-0.3.30
                                              rust-futures-task-0.3.30
                                              rust-futures-util-0.3.30
                                              rust-gimli-0.29.0
                                              rust-h2-0.3.26
                                              rust-hashbrown-0.14.5
                                              rust-hermit-abi-0.3.9
                                              rust-http-0.2.12
                                              rust-http-body-0.4.6
                                              rust-httparse-1.9.4
                                              rust-httpdate-1.0.3
                                              rust-hyper-0.14.29
                                              rust-hyper-tls-0.5.0
                                              rust-iana-time-zone-0.1.60
                                              rust-iana-time-zone-haiku-0.1.2
                                              rust-idna-0.5.0
                                              rust-indexmap-2.2.6
                                              rust-ipnet-2.9.0
                                              rust-itertools-0.5.10
                                              rust-itoa-1.0.11
                                              rust-js-sys-0.3.69
                                              rust-lazy-static-1.5.0
                                              rust-libc-0.2.155
                                              rust-linux-raw-sys-0.4.14
                                              rust-lock-api-0.4.12
                                              rust-log-0.4.21
                                              rust-lsp-types-0.94.1
                                              rust-memchr-2.7.4
                                              rust-mime-0.3.17
                                              rust-miniz-oxide-0.7.4
                                              rust-mio-0.8.11
                                              rust-native-tls-0.2.12
                                              rust-num-conv-0.1.0
                                              rust-num-traits-0.2.19
                                              rust-num-cpus-1.16.0
                                              rust-object-0.36.0
                                              rust-once-cell-1.19.0
                                              rust-openssl-0.10.64
                                              rust-openssl-macros-0.1.1
                                              rust-openssl-probe-0.1.5
                                              rust-openssl-sys-0.9.102
                                              rust-parking-lot-0.12.3
                                              rust-parking-lot-core-0.9.10
                                              rust-percent-encoding-2.3.1
                                              rust-pin-project-1.1.5
                                              rust-pin-project-internal-1.1.5
                                              rust-pin-project-lite-0.2.14
                                              rust-pin-utils-0.1.0
                                              rust-pkg-config-0.3.30
                                              rust-powerfmt-0.2.0
                                              rust-proc-macro2-1.0.86
                                              rust-quote-1.0.36
                                              rust-redox-syscall-0.5.2
                                              rust-regex-1.10.5
                                              rust-regex-automata-0.4.7
                                              rust-regex-syntax-0.8.4
                                              rust-reqwest-0.11.27
                                              rust-rustc-demangle-0.1.24
                                              rust-rustix-0.38.34
                                              rust-rustls-pemfile-1.0.4
                                              rust-ryu-1.0.18
                                              rust-schannel-0.1.23
                                              rust-scopeguard-1.2.0
                                              rust-security-framework-2.11.0
                                              rust-security-framework-sys-2.11.0
                                              rust-semver-rs-0.2.0
                                              rust-serde-1.0.203
                                              rust-serde-derive-1.0.203
                                              rust-serde-json-1.0.117
                                              rust-serde-repr-0.1.19
                                              rust-serde-urlencoded-0.7.1
                                              rust-signal-hook-registry-1.4.2
                                              rust-slab-0.4.9
                                              rust-smallvec-1.13.2
                                              rust-socket2-0.5.7
                                              rust-syn-2.0.87
                                              rust-sync-wrapper-0.1.2
                                              rust-system-configuration-0.5.1
                                              rust-system-configuration-sys-0.5.0
                                              rust-tempfile-3.10.1
                                              rust-thiserror-1.0.68
                                              rust-thiserror-impl-1.0.68
                                              rust-time-0.3.36
                                              rust-time-core-0.1.2
                                              rust-time-macros-0.2.18
                                              rust-tinyvec-1.6.0
                                              rust-tinyvec-macros-0.1.1
                                              rust-tokio-1.38.0
                                              rust-tokio-macros-2.3.0
                                              rust-tokio-native-tls-0.3.1
                                              rust-tokio-util-0.7.11
                                              rust-tower-0.4.13
                                              rust-tower-layer-0.3.2
                                              rust-tower-lsp-0.20.0
                                              rust-tower-lsp-macros-0.9.0
                                              rust-tower-service-0.3.2
                                              rust-tracing-0.1.40
                                              rust-tracing-attributes-0.1.27
                                              rust-tracing-core-0.1.32
                                              rust-tree-sitter-0.22.6
                                              rust-tree-sitter-json-0.21.0
                                              rust-try-lock-0.2.5
                                              rust-unicase-2.8.0
                                              rust-unicode-bidi-0.3.15
                                              rust-unicode-ident-1.0.12
                                              rust-unicode-normalization-0.1.23
                                              rust-url-2.5.2
                                              rust-urlencoding-2.1.3
                                              rust-vcpkg-0.2.15
                                              rust-want-0.3.1
                                              rust-wasi-0.11.0+wasi-snapshot-preview1
                                              rust-wasm-bindgen-0.2.92
                                              rust-wasm-bindgen-backend-0.2.92
                                              rust-wasm-bindgen-futures-0.4.42
                                              rust-wasm-bindgen-macro-0.2.92
                                              rust-wasm-bindgen-macro-support-0.2.92
                                              rust-wasm-bindgen-shared-0.2.92
                                              rust-web-sys-0.3.69
                                              rust-windows-core-0.52.0
                                              rust-windows-sys-0.48.0
                                              rust-windows-sys-0.52.0
                                              rust-windows-targets-0.48.5
                                              rust-windows-targets-0.52.5
                                              rust-windows-aarch64-gnullvm-0.48.5
                                              rust-windows-aarch64-gnullvm-0.52.5
                                              rust-windows-aarch64-msvc-0.48.5
                                              rust-windows-aarch64-msvc-0.52.5
                                              rust-windows-i686-gnu-0.48.5
                                              rust-windows-i686-gnu-0.52.5
                                              rust-windows-i686-gnullvm-0.52.5
                                              rust-windows-i686-msvc-0.48.5
                                              rust-windows-i686-msvc-0.52.5
                                              rust-windows-x86-64-gnu-0.48.5
                                              rust-windows-x86-64-gnu-0.52.5
                                              rust-windows-x86-64-gnullvm-0.48.5
                                              rust-windows-x86-64-gnullvm-0.52.5
                                              rust-windows-x86-64-msvc-0.48.5
                                              rust-windows-x86-64-msvc-0.52.5
                                              rust-winreg-0.50.0))
                     (pimsync =>
                              (list rust-addr2line-0.25.1
                                    rust-adler2-2.0.1
                                    rust-ahash-0.8.12
                                    rust-allocator-api2-0.2.21
                                    rust-android-system-properties-0.1.5
                                    rust-anyhow-1.0.100
                                    rust-async-lock-3.4.1
                                    rust-async-trait-0.1.89
                                    rust-atomic-waker-1.1.2
                                    rust-autocfg-1.5.0
                                    rust-backtrace-0.3.76
                                    rust-base64-0.22.1
                                    rust-bitflags-2.9.4
                                    rust-block-buffer-0.10.4
                                    rust-bumpalo-3.19.0
                                    rust-bytes-1.10.1
                                    rust-calcard-0.2.0
                                    rust-camino-1.2.1
                                    rust-cc-1.2.40
                                    rust-cfg-if-1.0.3
                                    rust-chrono-0.4.42
                                    rust-chrono-tz-0.10.4
                                    rust-colored-2.2.0
                                    rust-concurrent-queue-2.5.0
                                    rust-core-foundation-0.10.1
                                    rust-core-foundation-sys-0.8.7
                                    rust-cpufeatures-0.2.17
                                    rust-crossbeam-channel-0.5.15
                                    rust-crossbeam-epoch-0.9.18
                                    rust-crossbeam-utils-0.8.21
                                    rust-crypto-common-0.1.6
                                    rust-deranged-0.5.4
                                    rust-digest-0.10.7
                                    rust-domain-0.11.0
                                    rust-domain-macros-0.11.0
                                    rust-encoding-rs-0.8.35
                                    rust-equivalent-1.0.2
                                    rust-errno-0.3.14
                                    rust-event-listener-5.4.1
                                    rust-event-listener-strategy-0.5.4
                                    rust-fastrand-2.3.0
                                    rust-find-msvc-tools-0.1.3
                                    rust-fnv-1.0.7
                                    rust-futures-channel-0.3.31
                                    rust-futures-core-0.3.31
                                    rust-futures-macro-0.3.31
                                    rust-futures-task-0.3.31
                                    rust-futures-util-0.3.31
                                    rust-generic-array-0.14.7
                                    rust-getrandom-0.2.16
                                    rust-getrandom-0.3.3
                                    rust-gimli-0.32.3
                                    rust-hashbrown-0.14.5
                                    rust-hashify-0.2.7
                                    rust-hex-0.4.3
                                    rust-http-1.3.1
                                    rust-http-body-1.0.1
                                    rust-http-body-util-0.1.3
                                    rust-httparse-1.10.1
                                    rust-httpdate-1.0.3
                                    rust-hyper-1.7.0
                                    rust-hyper-rustls-0.27.7
                                    rust-hyper-util-0.1.17
                                    rust-hyperlocal-0.9.1
                                    rust-iana-time-zone-0.1.64
                                    rust-iana-time-zone-haiku-0.1.2
                                    rust-inotify-0.11.0
                                    rust-inotify-sys-0.1.5
                                    rust-io-uring-0.7.10
                                    rust-itoa-1.0.15
                                    rust-jmap-tools-0.1.2
                                    rust-js-sys-0.3.81
                                    rust-lazy-static-1.5.0
                                    rust-lexopt-0.3.1
                                    rust-libc-0.2.176
                                    rust-libdav-0.10.0
                                    rust-libjmap-0.1.1
                                    rust-linux-raw-sys-0.11.0
                                    rust-lock-api-0.4.14
                                    rust-log-0.4.28
                                    rust-mail-builder-0.4.4
                                    rust-mail-parser-0.11.1
                                    rust-memchr-2.7.6
                                    rust-mime-0.3.17
                                    rust-miniz-oxide-0.8.9
                                    rust-mio-1.0.4
                                    rust-moka-0.12.11
                                    rust-num-conv-0.1.0
                                    rust-num-traits-0.2.19
                                    rust-object-0.37.3
                                    rust-octseq-0.5.2
                                    rust-once-cell-1.21.3
                                    rust-openssl-probe-0.1.6
                                    rust-parking-2.2.1
                                    rust-parking-lot-0.12.5
                                    rust-parking-lot-core-0.9.12
                                    rust-phf-0.12.1
                                    rust-phf-shared-0.12.1
                                    rust-pin-project-lite-0.2.16
                                    rust-pin-utils-0.1.0
                                    rust-pkg-config-0.3.32
                                    rust-portable-atomic-1.11.1
                                    rust-powerfmt-0.2.0
                                    rust-ppv-lite86-0.2.21
                                    rust-proc-macro2-1.0.101
                                    rust-quote-1.0.41
                                    rust-r-efi-5.3.0
                                    rust-rand-0.8.5
                                    rust-rand-0.9.2
                                    rust-rand-chacha-0.3.1
                                    rust-rand-chacha-0.9.0
                                    rust-rand-core-0.6.4
                                    rust-rand-core-0.9.3
                                    rust-redox-syscall-0.5.18
                                    rust-ring-0.17.14
                                    rust-roxmltree-0.21.0
                                    rust-rustc-demangle-0.1.26
                                    rust-rustc-version-0.4.1
                                    rust-rustix-1.1.2
                                    rust-rustls-0.23.32
                                    rust-rustls-native-certs-0.8.1
                                    rust-rustls-pemfile-2.2.0
                                    rust-rustls-pki-types-1.12.0
                                    rust-rustls-webpki-0.103.7
                                    rust-rustversion-1.0.22
                                    rust-ryu-1.0.20
                                    rust-scfg-0.3.1
                                    rust-schannel-0.1.28
                                    rust-scopeguard-1.2.0
                                    rust-security-framework-3.5.1
                                    rust-security-framework-sys-2.15.0
                                    rust-semver-1.0.27
                                    rust-serde-1.0.228
                                    rust-serde-core-1.0.228
                                    rust-serde-derive-1.0.228
                                    rust-serde-json-1.0.145
                                    rust-sha1-smol-1.0.1
                                    rust-sha2-0.10.9
                                    rust-shell-words-1.1.0
                                    rust-shlex-1.3.0
                                    rust-simple-logger-5.0.0
                                    rust-siphasher-1.0.1
                                    rust-slab-0.4.11
                                    rust-smallvec-1.15.1
                                    rust-socket2-0.6.0
                                    rust-sqlite-0.37.0
                                    rust-sqlite3-src-0.7.0
                                    rust-sqlite3-sys-0.18.0
                                    rust-subtle-2.6.1
                                    rust-syn-2.0.106
                                    rust-sync-wrapper-1.0.2
                                    rust-tagptr-0.2.0
                                    rust-tempfile-3.23.0
                                    rust-thiserror-2.0.17
                                    rust-thiserror-impl-2.0.17
                                    rust-time-0.3.44
                                    rust-time-core-0.1.6
                                    rust-tokio-1.47.1
                                    rust-tokio-macros-2.5.0
                                    rust-tokio-rustls-0.26.4
                                    rust-tower-0.5.2
                                    rust-tower-http-0.6.6
                                    rust-tower-layer-0.3.3
                                    rust-tower-service-0.3.3
                                    rust-tracing-0.1.41
                                    rust-tracing-attributes-0.1.30
                                    rust-tracing-core-0.1.34
                                    rust-try-lock-0.2.5
                                    rust-typenum-1.19.0
                                    rust-unicode-ident-1.0.19
                                    rust-untrusted-0.9.0
                                    rust-uuid-1.18.1
                                    rust-version-check-0.9.5
                                    rust-vparser-1.1.0
                                    rust-vstorage-0.4.0
                                    rust-want-0.3.1
                                    rust-wasi-0.11.1+wasi-snapshot-preview1
                                    rust-wasi-0.14.7+wasi-0.2.4
                                    rust-wasip2-1.0.1+wasi-0.2.4
                                    rust-wasm-bindgen-0.2.104
                                    rust-wasm-bindgen-backend-0.2.104
                                    rust-wasm-bindgen-macro-0.2.104
                                    rust-wasm-bindgen-macro-support-0.2.104
                                    rust-wasm-bindgen-shared-0.2.104
                                    rust-windows-core-0.62.2
                                    rust-windows-implement-0.60.2
                                    rust-windows-interface-0.59.3
                                    rust-windows-link-0.2.1
                                    rust-windows-result-0.4.1
                                    rust-windows-strings-0.5.1
                                    rust-windows-sys-0.48.0
                                    rust-windows-sys-0.52.0
                                    rust-windows-sys-0.59.0
                                    rust-windows-sys-0.61.2
                                    rust-windows-targets-0.48.5
                                    rust-windows-targets-0.52.6
                                    rust-windows-aarch64-gnullvm-0.48.5
                                    rust-windows-aarch64-gnullvm-0.52.6
                                    rust-windows-aarch64-msvc-0.48.5
                                    rust-windows-aarch64-msvc-0.52.6
                                    rust-windows-i686-gnu-0.48.5
                                    rust-windows-i686-gnu-0.52.6
                                    rust-windows-i686-gnullvm-0.52.6
                                    rust-windows-i686-msvc-0.48.5
                                    rust-windows-i686-msvc-0.52.6
                                    rust-windows-x86-64-gnu-0.48.5
                                    rust-windows-x86-64-gnu-0.52.6
                                    rust-windows-x86-64-gnullvm-0.48.5
                                    rust-windows-x86-64-gnullvm-0.52.6
                                    rust-windows-x86-64-msvc-0.48.5
                                    rust-windows-x86-64-msvc-0.52.6
                                    rust-wit-bindgen-0.46.0
                                    rust-zerocopy-0.8.27
                                    rust-zerocopy-derive-0.8.27
                                    rust-zeroize-1.8.2))
                     (rioterm =>
                              (list rust-ab-glyph-0.2.31
                               rust-ab-glyph-rasterizer-0.1.10
                               rust-adler2-2.0.1
                               rust-adler32-1.2.0
                               rust-ahash-0.8.12
                               rust-aho-corasick-1.1.3
                               rust-allocator-api2-0.2.21
                               rust-android-system-properties-0.1.5
                               rust-anes-0.1.6
                               rust-anstream-0.6.20
                               rust-anstyle-1.0.11
                               rust-anstyle-parse-0.2.7
                               rust-anstyle-query-1.1.4
                               rust-anstyle-wincon-3.0.10
                               rust-approx-0.5.1
                               rust-arc-swap-1.7.1
                               rust-array-concat-0.5.5
                               rust-arrayref-0.3.9
                               rust-arrayvec-0.7.6
                               rust-as-raw-xcb-connection-1.0.1
                               rust-ash-0.38.0+1.3.281
                               rust-atomic-waker-1.1.2
                               rust-autocfg-1.5.0
                               rust-base64-0.22.1
                               rust-bincode-2.0.1
                               rust-bincode-derive-2.0.1
                               rust-bindgen-0.70.1
                               rust-bit-set-0.8.0
                               rust-bit-vec-0.8.0
                               rust-bitflags-1.3.2
                               rust-bitflags-2.9.2
                               rust-blake3-1.8.2
                               rust-block-0.1.6
                               rust-block2-0.5.1
                               rust-borsh-1.5.7
                               rust-bumpalo-3.19.0
                               rust-bytecount-0.6.9
                               rust-bytemuck-1.23.2
                               rust-bytemuck-derive-1.10.1
                               rust-byteorder-lite-0.1.0
                               rust-bytes-0.3.0
                               rust-calloop-0.13.0
                               rust-calloop-wayland-source-0.3.0
                               rust-cast-0.3.0
                               rust-cc-1.2.33
                               rust-cexpr-0.6.0
                               rust-cfg-if-0.1.10
                               rust-cfg-if-1.0.1
                               rust-cfg-aliases-0.2.1
                               rust-ciborium-0.2.2
                               rust-ciborium-io-0.2.2
                               rust-ciborium-ll-0.2.2
                               rust-clang-sys-1.8.1
                               rust-clap-4.5.45
                               rust-clap-builder-4.5.44
                               rust-clap-derive-4.5.45
                               rust-clap-lex-0.7.5
                               rust-clipboard-win-5.4.1
                               rust-codespan-reporting-0.12.0
                               rust-color-quant-1.1.0
                               rust-colorchoice-1.0.4
                               rust-concurrent-queue-2.5.0
                               rust-console-error-panic-hook-0.1.7
                               rust-console-log-0.2.2
                               rust-constant-time-eq-0.3.1
                               rust-copypasta-0.10.2
                               rust-core-foundation-0.9.4
                               rust-core-foundation-0.10.1
                               rust-core-foundation-sys-0.8.7
                               rust-core-graphics-0.24.0
                               rust-core-graphics-types-0.1.3
                               rust-core-graphics-types-0.2.0
                               rust-core-maths-0.1.1
                               rust-crc-3.3.0
                               rust-crc-catalog-2.4.0
                               rust-crc32fast-1.5.0
                               rust-criterion-0.6.0
                               rust-criterion-plot-0.5.0
                               rust-crossbeam-deque-0.8.6
                               rust-crossbeam-epoch-0.9.18
                               rust-crossbeam-utils-0.8.21
                               rust-crunchy-0.2.4
                               rust-ctor-lite-0.1.0
                               rust-cursor-icon-1.2.0
                               rust-data-encoding-2.9.0
                               rust-deflate-1.0.0
                               rust-diff-0.1.13
                               rust-dirs-6.0.0
                               rust-dirs-next-1.0.2
                               rust-dirs-sys-0.5.0
                               rust-dirs-sys-next-0.1.2
                               rust-dispatch-0.2.0
                               rust-displaydoc-0.2.5
                               rust-dlib-0.5.2
                               rust-document-features-0.2.11
                               rust-downcast-rs-1.2.1
                               rust-dpi-0.1.2
                               rust-dunce-1.0.5
                               rust-either-1.15.0
                               rust-encoding-rs-0.8.35
                               rust-env-logger-0.4.3
                               rust-equivalent-1.0.2
                               rust-errno-0.3.13
                               rust-error-code-3.3.2
                               rust-euclid-0.22.11
                               rust-fastrand-2.3.0
                               rust-fdeflate-0.3.7
                               rust-fixedbitset-0.5.7
                               rust-flate2-1.1.2
                               rust-foldhash-0.1.5
                               rust-font-types-0.9.0
                               rust-fontconfig-parser-0.5.8
                               rust-foreign-types-0.5.0
                               rust-foreign-types-macros-0.2.3
                               rust-foreign-types-shared-0.3.1
                               rust-form-urlencoded-1.2.1
                               rust-fs2-0.4.3
                               rust-fsevent-sys-4.1.0
                               rust-fuchsia-cprng-0.1.1
                               rust-fuchsia-zircon-0.3.3
                               rust-fuchsia-zircon-sys-0.3.3
                               rust-futures-0.3.31
                               rust-futures-channel-0.3.31
                               rust-futures-core-0.3.31
                               rust-futures-executor-0.3.31
                               rust-futures-io-0.3.31
                               rust-futures-macro-0.3.31
                               rust-futures-sink-0.3.31
                               rust-futures-task-0.3.31
                               rust-futures-util-0.3.31
                               rust-gethostname-0.4.3
                               rust-getrandom-0.2.16
                               rust-getrandom-0.3.3
                               rust-gif-0.13.3
                               rust-gl-generator-0.14.0
                               rust-glob-0.3.3
                               rust-glow-0.16.0
                               rust-glslang-0.6.2
                               rust-glslang-sys-0.7.0+1062752
                               rust-glutin-wgl-sys-0.6.1
                               rust-gpu-alloc-0.6.0
                               rust-gpu-alloc-types-0.3.0
                               rust-gpu-allocator-0.27.0
                               rust-gpu-descriptor-0.3.2
                               rust-gpu-descriptor-types-0.2.0
                               rust-guillotiere-0.6.2
                               rust-half-2.6.0
                               rust-halfbrown-0.2.5
                               rust-halfbrown-0.3.0
                               rust-hashbrown-0.14.5
                               rust-hashbrown-0.15.5
                               rust-heck-0.5.0
                               rust-hermit-abi-0.5.2
                               rust-hexf-parse-0.2.1
                               rust-icu-collections-2.0.0
                               rust-icu-locale-core-2.0.0
                               rust-icu-normalizer-2.0.0
                               rust-icu-normalizer-data-2.0.0
                               rust-icu-properties-2.0.1
                               rust-icu-properties-data-2.0.1
                               rust-icu-provider-2.0.0
                               rust-idna-1.0.3
                               rust-idna-adapter-1.2.1
                               rust-image-0.25.6
                               rust-image-webp-0.2.3
                               rust-indexmap-2.10.0
                               rust-inotify-0.11.0
                               rust-inotify-sys-0.1.5
                               rust-iovec-0.1.4
                               rust-is-terminal-polyfill-1.70.1
                               rust-itertools-0.10.5
                               rust-itertools-0.13.0
                               rust-itoa-1.0.15
                               rust-jni-sys-0.3.0
                               rust-jobserver-0.1.33
                               rust-jpeg-decoder-0.3.2
                               rust-js-sys-0.3.77
                               rust-khronos-egl-6.0.0
                               rust-khronos-api-3.1.0
                               rust-kqueue-1.1.1
                               rust-kqueue-sys-1.0.4
                               rust-lazy-static-1.5.0
                               rust-libc-0.2.175
                               rust-libloading-0.8.8
                               rust-libm-0.2.15
                               rust-librashader-cache-0.8.1
                               rust-librashader-common-0.8.1
                               rust-librashader-pack-0.8.1
                               rust-librashader-preprocess-0.8.1
                               rust-librashader-presets-0.8.1
                               rust-librashader-reflect-0.8.1
                               rust-librashader-runtime-0.8.1
                               rust-libredox-0.1.9
                               rust-linked-hash-map-0.5.6
                               rust-linux-raw-sys-0.4.15
                               rust-linux-raw-sys-0.9.4
                               rust-litemap-0.8.0
                               rust-litrs-0.4.2
                               rust-lock-api-0.4.13
                               rust-log-0.3.9
                               rust-log-0.4.27
                               rust-lru-0.16.0
                               rust-malloc-buf-0.0.6
                               rust-matchers-0.1.0
                               rust-memchr-2.7.5
                               rust-memmap2-0.9.7
                               rust-metal-0.31.0
                               rust-minicov-0.3.7
                               rust-minimal-lexical-0.2.1
                               rust-miniz-oxide-0.8.9
                               rust-mio-1.0.4
                               rust-miow-0.5.0
                               rust-miow-0.6.0
                               rust-naga-25.0.1
                               rust-ndk-sys-0.5.0+25.2.9519653
                               rust-net2-0.2.39
                               rust-nom-7.1.3
                               rust-nom-8.0.0
                               rust-nom-locate-5.0.0
                               rust-notify-8.2.0
                               rust-notify-types-2.0.0
                               rust-nu-ansi-term-0.46.0
                               rust-num-derive-0.4.2
                               rust-num-traits-0.2.19
                               rust-objc-0.2.7
                               rust-objc-rs-0.2.8
                               rust-objc-sys-0.3.5
                               rust-objc2-0.5.2
                               rust-objc2-app-kit-0.2.2
                               rust-objc2-core-data-0.2.2
                               rust-objc2-core-image-0.2.2
                               rust-objc2-encode-4.1.0
                               rust-objc2-foundation-0.2.2
                               rust-objc2-metal-0.2.2
                               rust-objc2-quartz-core-0.2.2
                               rust-once-cell-1.21.3
                               rust-once-cell-polyfill-1.70.1
                               rust-oorandom-11.1.5
                               rust-option-ext-0.2.0
                               rust-orbclient-0.3.48
                               rust-ordered-float-4.6.0
                               rust-overload-0.1.1
                               rust-owned-ttf-parser-0.25.1
                               rust-parking-lot-0.12.4
                               rust-parking-lot-core-0.9.11
                               rust-paste-1.0.15
                               rust-percent-encoding-2.3.1
                               rust-persy-1.6.0
                               rust-petgraph-0.8.2
                               rust-pin-project-1.1.10
                               rust-pin-project-internal-1.1.10
                               rust-pin-project-lite-0.2.16
                               rust-pin-utils-0.1.0
                               rust-pkg-config-0.3.32
                               rust-platform-dirs-0.3.0
                               rust-plotters-0.3.7
                               rust-plotters-backend-0.3.7
                               rust-plotters-svg-0.3.7
                               rust-png-0.17.16
                               rust-polling-3.10.0
                               rust-portable-atomic-1.11.1
                               rust-potential-utf-0.1.2
                               rust-ppv-lite86-0.2.21
                               rust-presser-0.3.1
                               rust-pretty-assertions-1.4.1
                               rust-prettyplease-0.2.36
                               rust-proc-macro2-1.0.101
                               rust-profiling-1.0.17
                               rust-quick-error-2.0.1
                               rust-quick-xml-0.37.5
                               rust-quote-1.0.40
                               rust-r-efi-5.3.0
                               rust-rand-0.4.6
                               rust-rand-0.8.5
                               rust-rand-chacha-0.3.1
                               rust-rand-core-0.3.1
                               rust-rand-core-0.4.2
                               rust-rand-core-0.6.4
                               rust-range-alloc-0.1.4
                               rust-raw-window-handle-0.6.2
                               rust-rayon-1.11.0
                               rust-rayon-core-1.13.0
                               rust-rdrand-0.4.0
                               rust-read-fonts-0.31.3
                               rust-redox-syscall-0.4.1
                               rust-redox-syscall-0.5.17
                               rust-redox-users-0.4.6
                               rust-redox-users-0.5.2
                               rust-regex-1.11.1
                               rust-regex-automata-0.1.10
                               rust-regex-automata-0.4.9
                               rust-regex-syntax-0.6.29
                               rust-regex-syntax-0.8.5
                               rust-remove-dir-all-0.5.3
                               rust-renderdoc-sys-1.1.0
                               rust-roxmltree-0.20.0
                               rust-rspirv-0.12.0+sdk-1.3.268.0
                               rust-rustc-hash-1.1.0
                               rust-rustc-hash-2.1.1
                               rust-rustix-0.38.44
                               rust-rustix-1.0.8
                               rust-rustversion-1.0.22
                               rust-ryu-1.0.20
                               rust-same-file-1.0.6
                               rust-scoped-tls-1.0.1
                               rust-scopeguard-1.2.0
                               rust-sctk-adwaita-0.10.1
                               rust-serde-1.0.219
                               rust-serde-derive-1.0.219
                               rust-serde-json-1.0.142
                               rust-serde-spanned-1.0.0
                               rust-sharded-slab-0.1.7
                               rust-shlex-1.3.0
                               rust-signal-hook-0.3.18
                               rust-signal-hook-registry-1.4.6
                               rust-simd-adler32-0.3.7
                               rust-simdutf8-0.1.5
                               rust-skrifa-0.33.2
                               rust-slab-0.4.11
                               rust-slotmap-1.0.7
                               rust-smallvec-1.15.1
                               rust-smartstring-1.0.1
                               rust-smithay-client-toolkit-0.19.2
                               rust-smithay-clipboard-0.7.2
                               rust-smol-str-0.3.2
                               rust-softbuffer-0.4.6
                               rust-spirv-0.3.0+sdk-1.3.268.0
                               rust-spirv-cross-sys-0.4.3+e670b39
                               rust-spirv-cross2-0.4.6
                               rust-spirv-cross2-derive-0.1.0
                               rust-stable-deref-trait-1.2.0
                               rust-static-assertions-1.1.0
                               rust-strict-num-0.1.1
                               rust-strsim-0.11.1
                               rust-strum-0.26.3
                               rust-strum-macros-0.26.4
                               rust-svg-fmt-0.4.5
                               rust-syn-2.0.106
                               rust-synstructure-0.13.2
                               rust-tempdir-0.3.7
                               rust-termcolor-1.4.1
                               rust-thiserror-1.0.69
                               rust-thiserror-2.0.15
                               rust-thiserror-impl-1.0.69
                               rust-thiserror-impl-2.0.15
                               rust-thread-local-1.1.9
                               rust-tiff-0.9.1
                               rust-tiny-skia-0.11.4
                               rust-tiny-skia-path-0.11.4
                               rust-tiny-xlib-0.2.4
                               rust-tinystr-0.8.1
                               rust-tinytemplate-1.2.1
                               rust-tinyvec-1.9.0
                               rust-tinyvec-macros-0.1.1
                               rust-toml-0.5.11
                               rust-toml-0.9.5
                               rust-toml-datetime-0.7.0
                               rust-toml-parser-1.0.2
                               rust-toml-writer-1.0.2
                               rust-tracing-0.1.41
                               rust-tracing-attributes-0.1.30
                               rust-tracing-core-0.1.34
                               rust-tracing-log-0.2.0
                               rust-tracing-subscriber-0.3.19
                               rust-tracing-web-0.1.3
                               rust-ttf-parser-0.25.1
                               rust-twox-hash-2.1.1
                               rust-unicode-ident-1.0.18
                               rust-unicode-segmentation-1.12.0
                               rust-unicode-width-0.2.1
                               rust-unicode-width-16-0.1.0
                               rust-unsigned-varint-0.8.0
                               rust-unty-0.0.4
                               rust-url-2.5.4
                               rust-utf8-iter-1.0.4
                               rust-utf8parse-0.2.2
                               rust-valuable-0.1.1
                               rust-vec-extract-if-polyfill-0.1.0
                               rust-version-check-0.9.5
                               rust-virtue-0.0.18
                               rust-walkdir-2.5.0
                               rust-wasi-0.11.1+wasi-snapshot-preview1
                               rust-wasi-0.14.2+wasi-0.2.4
                               rust-wasm-bindgen-0.2.100
                               rust-wasm-bindgen-backend-0.2.100
                               rust-wasm-bindgen-futures-0.4.50
                               rust-wasm-bindgen-macro-0.2.100
                               rust-wasm-bindgen-macro-support-0.2.100
                               rust-wasm-bindgen-shared-0.2.100
                               rust-wasm-bindgen-test-0.3.50
                               rust-wasm-bindgen-test-macro-0.3.50
                               rust-wayland-backend-0.3.11
                               rust-wayland-client-0.31.11
                               rust-wayland-csd-frame-0.3.0
                               rust-wayland-cursor-0.31.11
                               rust-wayland-protocols-0.32.9
                               rust-wayland-protocols-plasma-0.3.9
                               rust-wayland-protocols-wlr-0.3.9
                               rust-wayland-scanner-0.31.7
                               rust-wayland-sys-0.31.7
                               rust-web-sys-0.3.77
                               rust-web-time-1.1.0
                               rust-weezl-0.1.10
                               rust-wgpu-25.0.2
                               rust-wgpu-core-25.0.2
                               rust-wgpu-core-deps-apple-25.0.0
                               rust-wgpu-core-deps-emscripten-25.0.0
                               rust-wgpu-core-deps-windows-linux-android-25.0.0
                               rust-wgpu-hal-25.0.2
                               rust-wgpu-types-25.0.0
                               rust-winapi-0.3.9
                               rust-winapi-i686-pc-windows-gnu-0.4.0
                               rust-winapi-util-0.1.9
                               rust-winapi-x86-64-pc-windows-gnu-0.4.0
                               rust-windows-0.42.0
                               rust-windows-0.58.0
                               rust-windows-core-0.58.0
                               rust-windows-implement-0.58.0
                               rust-windows-interface-0.58.0
                               rust-windows-link-0.1.3
                               rust-windows-result-0.2.0
                               rust-windows-strings-0.1.0
                               rust-windows-sys-0.42.0
                               rust-windows-sys-0.48.0
                               rust-windows-sys-0.59.0
                               rust-windows-sys-0.60.2
                               rust-windows-targets-0.48.5
                               rust-windows-targets-0.52.6
                               rust-windows-targets-0.53.3
                               rust-windows-aarch64-gnullvm-0.42.2
                               rust-windows-aarch64-gnullvm-0.48.5
                               rust-windows-aarch64-gnullvm-0.52.6
                               rust-windows-aarch64-gnullvm-0.53.0
                               rust-windows-aarch64-msvc-0.42.2
                               rust-windows-aarch64-msvc-0.48.5
                               rust-windows-aarch64-msvc-0.52.6
                               rust-windows-aarch64-msvc-0.53.0
                               rust-windows-i686-gnu-0.42.2
                               rust-windows-i686-gnu-0.48.5
                               rust-windows-i686-gnu-0.52.6
                               rust-windows-i686-gnu-0.53.0
                               rust-windows-i686-gnullvm-0.52.6
                               rust-windows-i686-gnullvm-0.53.0
                               rust-windows-i686-msvc-0.42.2
                               rust-windows-i686-msvc-0.48.5
                               rust-windows-i686-msvc-0.52.6
                               rust-windows-i686-msvc-0.53.0
                               rust-windows-x86-64-gnu-0.42.2
                               rust-windows-x86-64-gnu-0.48.5
                               rust-windows-x86-64-gnu-0.52.6
                               rust-windows-x86-64-gnu-0.53.0
                               rust-windows-x86-64-gnullvm-0.42.2
                               rust-windows-x86-64-gnullvm-0.48.5
                               rust-windows-x86-64-gnullvm-0.52.6
                               rust-windows-x86-64-gnullvm-0.53.0
                               rust-windows-x86-64-msvc-0.42.2
                               rust-windows-x86-64-msvc-0.48.5
                               rust-windows-x86-64-msvc-0.52.6
                               rust-windows-x86-64-msvc-0.53.0
                               rust-winnow-0.7.12
                               rust-winres-0.1.12
                               rust-wit-bindgen-rt-0.39.0
                               rust-writeable-0.6.1
                               rust-x11-clipboard-0.9.3
                               rust-x11-dl-2.21.0
                               rust-x11rb-0.13.1
                               rust-x11rb-protocol-0.13.1
                               rust-xcursor-0.3.10
                               rust-xkbcommon-dl-0.4.2
                               rust-xkeysym-0.2.1
                               rust-xml-rs-0.8.27
                               rust-yansi-1.0.1
                               rust-yazi-0.2.1
                               rust-yoke-0.8.0
                               rust-yoke-derive-0.8.0
                               rust-zeno-0.3.3
                               rust-zerocopy-0.8.26
                               rust-zerocopy-derive-0.8.26
                               rust-zerofrom-0.1.6
                               rust-zerofrom-derive-0.1.6
                               rust-zerotrie-0.2.2
                               rust-zerovec-0.11.4
                               rust-zerovec-derive-0.11.1
                               rust-zigzag-0.1.0
                               rust-zune-core-0.4.12
                               rust-zune-jpeg-0.4.20))
                     (sniffnet =>
                               (list rust-ab-glyph-0.2.32
                                rust-ab-glyph-rasterizer-0.1.10
                                rust-adler2-2.0.1
                                rust-ahash-0.7.8
                                rust-ahash-0.8.12
                                rust-aho-corasick-1.1.4
                                rust-aliasable-0.1.3
                                rust-allocator-api2-0.2.21
                                rust-alsa-0.9.1
                                rust-alsa-sys-0.3.1
                                rust-android-activity-0.6.0
                                rust-android-properties-0.2.2
                                rust-android-system-properties-0.1.5
                                rust-anstream-0.6.21
                                rust-anstyle-1.0.13
                                rust-anstyle-parse-0.2.7
                                rust-anstyle-query-1.1.4
                                rust-anstyle-wincon-3.0.10
                                rust-approx-0.5.1
                                rust-arrayref-0.3.9
                                rust-arrayvec-0.7.6
                                rust-as-raw-xcb-connection-1.0.1
                                rust-ash-0.37.3+1.3.251
                                rust-ashpd-0.11.0
                                rust-async-broadcast-0.7.2
                                rust-async-channel-2.5.0
                                rust-async-executor-1.13.3
                                rust-async-fs-2.2.0
                                rust-async-io-2.6.0
                                rust-async-lock-3.4.1
                                rust-async-net-2.0.0
                                rust-async-process-2.5.0
                                rust-async-recursion-1.1.1
                                rust-async-signal-0.2.13
                                rust-async-task-4.7.1
                                rust-async-trait-0.1.89
                                rust-atomic-waker-1.1.2
                                rust-autocfg-1.5.0
                                rust-base64-0.22.1
                                rust-bit-set-0.5.3
                                rust-bit-vec-0.6.3
                                rust-bit-field-0.10.3
                                rust-bitflags-1.3.2
                                rust-bitflags-2.10.0
                                rust-block-0.1.6
                                rust-block-buffer-0.10.4
                                rust-block2-0.5.1
                                rust-block2-0.6.2
                                rust-blocking-1.6.2
                                rust-bumpalo-3.19.0
                                rust-by-address-1.2.1
                                rust-bytemuck-1.24.0
                                rust-bytemuck-derive-1.10.2
                                rust-byteorder-1.5.0
                                rust-bytes-1.10.1
                                rust-calloop-0.13.0
                                rust-calloop-wayland-source-0.3.0
                                rust-cc-1.2.44
                                rust-cesu8-1.1.0
                                rust-cfg-if-1.0.4
                                rust-cfg-aliases-0.1.1
                                rust-cfg-aliases-0.2.1
                                rust-chrono-0.4.42
                                rust-clap-4.5.51
                                rust-clap-builder-4.5.51
                                rust-clap-derive-4.5.49
                                rust-clap-lex-0.7.6
                                rust-clipboard-win-5.4.1
                                rust-clipboard-macos-0.1.1
                                rust-clipboard-wayland-0.2.2
                                rust-clipboard-x11-0.4.2
                                rust-codespan-reporting-0.11.1
                                rust-color-quant-1.1.0
                                rust-colorchoice-1.0.4
                                rust-com-0.6.0
                                rust-com-macros-0.6.0
                                rust-com-macros-support-0.6.0
                                rust-combine-4.6.7
                                rust-concurrent-queue-2.5.0
                                rust-confy-2.0.0
                                rust-core-foundation-0.9.4
                                rust-core-foundation-0.10.1
                                rust-core-foundation-sys-0.8.7
                                rust-core-graphics-0.23.2
                                rust-core-graphics-0.24.0
                                rust-core-graphics-types-0.1.3
                                rust-core-graphics-types-0.2.0
                                rust-coreaudio-rs-0.13.0
                                rust-cosmic-text-0.12.1
                                rust-cpal-0.16.0
                                rust-cpufeatures-0.2.17
                                rust-crc32fast-1.5.0
                                rust-crossbeam-deque-0.8.6
                                rust-crossbeam-epoch-0.9.18
                                rust-crossbeam-utils-0.8.21
                                rust-crunchy-0.2.4
                                rust-crypto-common-0.1.6
                                rust-ctor-lite-0.1.0
                                rust-ctrlc-3.5.1
                                rust-cursor-icon-1.2.0
                                rust-d3d12-0.19.0
                                rust-dark-light-1.1.1
                                rust-dasp-sample-0.11.0
                                rust-data-url-0.3.2
                                rust-dconf-rs-0.3.0
                                rust-detect-desktop-environment-0.2.0
                                rust-digest-0.10.7
                                rust-dirs-4.0.0
                                rust-dirs-sys-0.3.7
                                rust-dispatch-0.2.0
                                rust-dispatch2-0.3.0
                                rust-displaydoc-0.2.5
                                rust-dlib-0.5.2
                                rust-dlv-list-0.3.0
                                rust-dns-lookup-3.0.1
                                rust-doc-comment-0.3.4
                                rust-downcast-rs-1.2.1
                                rust-dpi-0.1.2
                                rust-drm-0.12.0
                                rust-drm-ffi-0.8.0
                                rust-drm-fourcc-2.2.0
                                rust-drm-sys-0.7.0
                                rust-either-1.15.0
                                rust-encoding-rs-0.8.35
                                rust-endi-1.1.0
                                rust-enumflags2-0.7.12
                                rust-enumflags2-derive-0.7.12
                                rust-equivalent-1.0.2
                                rust-errno-0.2.8
                                rust-errno-0.3.14
                                rust-errno-dragonfly-0.1.2
                                rust-error-code-3.3.2
                                rust-etagere-0.2.15
                                rust-etcetera-0.10.0
                                rust-etherparse-0.19.0
                                rust-euclid-0.22.11
                                rust-event-listener-5.4.1
                                rust-event-listener-strategy-0.5.4
                                rust-exr-1.73.0
                                rust-fast-srgb8-1.0.0
                                rust-fastrand-2.3.0
                                rust-fdeflate-0.3.7
                                rust-filedescriptor-0.8.3
                                rust-find-msvc-tools-0.1.4
                                rust-finl-unicode-1.4.0
                                rust-flate2-1.1.5
                                rust-float-cmp-0.9.0
                                rust-float-next-after-1.0.0
                                rust-fnv-1.0.7
                                rust-font-types-0.7.3
                                rust-fontconfig-parser-0.5.8
                                rust-fontdb-0.16.2
                                rust-fontdb-0.18.0
                                rust-foreign-types-0.3.2
                                rust-foreign-types-0.5.0
                                rust-foreign-types-macros-0.2.3
                                rust-foreign-types-shared-0.1.1
                                rust-foreign-types-shared-0.3.1
                                rust-form-urlencoded-1.2.2
                                rust-futures-0.3.31
                                rust-futures-channel-0.3.31
                                rust-futures-core-0.3.31
                                rust-futures-executor-0.3.31
                                rust-futures-io-0.3.31
                                rust-futures-lite-2.6.1
                                rust-futures-macro-0.3.31
                                rust-futures-sink-0.3.31
                                rust-futures-task-0.3.31
                                rust-futures-timer-3.0.3
                                rust-futures-util-0.3.31
                                rust-gag-1.0.0
                                rust-generic-array-0.14.9
                                rust-gethostname-1.1.0
                                rust-getrandom-0.2.16
                                rust-getrandom-0.3.4
                                rust-gif-0.13.3
                                rust-gl-generator-0.14.0
                                rust-glam-0.25.0
                                rust-glob-0.3.3
                                rust-glow-0.13.1
                                rust-glutin-wgl-sys-0.5.0
                                rust-gpu-alloc-0.6.0
                                rust-gpu-alloc-types-0.3.0
                                rust-gpu-allocator-0.25.0
                                rust-gpu-descriptor-0.2.4
                                rust-gpu-descriptor-types-0.1.2
                                rust-guillotiere-0.6.2
                                rust-h2-0.4.12
                                rust-half-2.7.1
                                rust-hashbrown-0.12.3
                                rust-hashbrown-0.14.5
                                rust-hashbrown-0.16.0
                                rust-hassle-rs-0.11.0
                                rust-heck-0.4.1
                                rust-heck-0.5.0
                                rust-hermit-abi-0.5.2
                                rust-hex-0.4.3
                                rust-hexf-parse-0.2.1
                                rust-home-0.5.12
                                rust-http-1.3.1
                                rust-http-body-1.0.1
                                rust-http-body-util-0.1.3
                                rust-httparse-1.10.1
                                rust-hyper-1.7.0
                                rust-hyper-rustls-0.27.7
                                rust-hyper-tls-0.6.0
                                rust-hyper-util-0.1.17
                                rust-iana-time-zone-0.1.64
                                rust-iana-time-zone-haiku-0.1.2
                                rust-iced-0.13.1
                                rust-iced-core-0.13.2
                                rust-iced-futures-0.13.2
                                rust-iced-glyphon-0.6.0
                                rust-iced-graphics-0.13.0
                                rust-iced-renderer-0.13.0
                                rust-iced-runtime-0.13.2
                                rust-iced-tiny-skia-0.13.0
                                rust-iced-wgpu-0.13.5
                                rust-iced-widget-0.13.4
                                rust-iced-winit-0.13.0
                                rust-icu-collections-2.1.1
                                rust-icu-locale-core-2.1.1
                                rust-icu-normalizer-2.1.1
                                rust-icu-normalizer-data-2.1.1
                                rust-icu-properties-2.1.1
                                rust-icu-properties-data-2.1.1
                                rust-icu-provider-2.1.1
                                rust-idna-1.1.0
                                rust-idna-adapter-1.2.1
                                rust-image-0.24.9
                                rust-imagesize-0.12.0
                                rust-indexmap-2.12.0
                                rust-instant-0.1.13
                                rust-ipnet-2.11.0
                                rust-ipnetwork-0.21.1
                                rust-iri-string-0.7.8
                                rust-is-terminal-polyfill-1.70.2
                                rust-itertools-0.10.5
                                rust-itoa-1.0.15
                                rust-jni-0.21.1
                                rust-jni-sys-0.3.0
                                rust-jobserver-0.1.34
                                rust-jpeg-decoder-0.3.2
                                rust-js-sys-0.3.82
                                rust-kamadak-exif-0.5.5
                                rust-khronos-egl-6.0.0
                                rust-khronos-api-3.1.0
                                rust-kurbo-0.10.4
                                rust-kurbo-0.11.3
                                rust-lazy-static-1.5.0
                                rust-lebe-0.5.3
                                rust-libc-0.2.177
                                rust-libloading-0.7.4
                                rust-libloading-0.8.9
                                rust-libm-0.2.15
                                rust-libredox-0.1.10
                                rust-linux-raw-sys-0.4.15
                                rust-linux-raw-sys-0.6.5
                                rust-linux-raw-sys-0.11.0
                                rust-litemap-0.8.1
                                rust-lock-api-0.4.14
                                rust-log-0.4.28
                                rust-lru-0.12.5
                                rust-lru-slab-0.1.2
                                rust-lyon-1.0.16
                                rust-lyon-algorithms-1.0.16
                                rust-lyon-geom-1.0.17
                                rust-lyon-path-1.0.16
                                rust-lyon-tessellation-1.0.16
                                rust-mach2-0.4.3
                                rust-malloc-buf-0.0.6
                                rust-maxminddb-0.26.0
                                rust-memchr-2.7.6
                                rust-memmap2-0.9.9
                                rust-memoffset-0.9.1
                                rust-metal-0.27.0
                                rust-mime-0.3.17
                                rust-miniz-oxide-0.8.9
                                rust-mio-1.1.0
                                rust-mutate-once-0.1.2
                                rust-naga-0.19.2
                                rust-native-tls-0.2.14
                                rust-ndk-0.9.0
                                rust-ndk-context-0.1.1
                                rust-ndk-sys-0.5.0+25.2.9519653
                                rust-ndk-sys-0.6.0+11769913
                                rust-nix-0.29.0
                                rust-nix-0.30.1
                                rust-num-bigint-0.4.6
                                rust-num-derive-0.4.2
                                rust-num-integer-0.1.46
                                rust-num-rational-0.4.2
                                rust-num-traits-0.2.19
                                rust-num-cpus-1.17.0
                                rust-num-enum-0.7.5
                                rust-num-enum-derive-0.7.5
                                rust-objc-0.2.7
                                rust-objc-sys-0.3.5
                                rust-objc2-0.5.2
                                rust-objc2-0.6.3
                                rust-objc2-app-kit-0.2.2
                                rust-objc2-app-kit-0.3.2
                                rust-objc2-audio-toolbox-0.3.2
                                rust-objc2-cloud-kit-0.2.2
                                rust-objc2-contacts-0.2.2
                                rust-objc2-core-audio-0.3.2
                                rust-objc2-core-audio-types-0.3.2
                                rust-objc2-core-data-0.2.2
                                rust-objc2-core-foundation-0.3.2
                                rust-objc2-core-image-0.2.2
                                rust-objc2-core-location-0.2.2
                                rust-objc2-encode-4.1.0
                                rust-objc2-foundation-0.2.2
                                rust-objc2-foundation-0.3.2
                                rust-objc2-link-presentation-0.2.2
                                rust-objc2-metal-0.2.2
                                rust-objc2-quartz-core-0.2.2
                                rust-objc2-symbols-0.2.2
                                rust-objc2-ui-kit-0.2.2
                                rust-objc2-uniform-type-identifiers-0.2.2
                                rust-objc2-user-notifications-0.2.2
                                rust-objc-exception-0.1.2
                                rust-once-cell-1.21.3
                                rust-once-cell-polyfill-1.70.2
                                rust-openssl-0.10.74
                                rust-openssl-macros-0.1.1
                                rust-openssl-probe-0.1.6
                                rust-openssl-sys-0.9.110
                                rust-orbclient-0.3.48
                                rust-ordered-multimap-0.4.3
                                rust-ordered-stream-0.2.0
                                rust-ouroboros-0.18.5
                                rust-ouroboros-macro-0.18.5
                                rust-owned-ttf-parser-0.25.1
                                rust-palette-0.7.6
                                rust-palette-derive-0.7.6
                                rust-parking-2.2.1
                                rust-parking-lot-0.11.2
                                rust-parking-lot-0.12.5
                                rust-parking-lot-core-0.8.6
                                rust-parking-lot-core-0.9.12
                                rust-paste-1.0.15
                                rust-pcap-2.3.0
                                rust-percent-encoding-2.3.2
                                rust-phf-0.11.3
                                rust-phf-0.13.1
                                rust-phf-codegen-0.13.1
                                rust-phf-generator-0.11.3
                                rust-phf-generator-0.13.1
                                rust-phf-macros-0.11.3
                                rust-phf-shared-0.11.3
                                rust-phf-shared-0.13.1
                                rust-pico-args-0.5.0
                                rust-pin-project-1.1.10
                                rust-pin-project-internal-1.1.10
                                rust-pin-project-lite-0.2.16
                                rust-pin-utils-0.1.0
                                rust-piper-0.2.4
                                rust-pkg-config-0.3.32
                                rust-plotters-0.3.7
                                rust-plotters-backend-0.3.7
                                rust-plotters-iced-0.11.0
                                rust-png-0.17.16
                                rust-polling-3.11.0
                                rust-pollster-0.4.0
                                rust-potential-utf-0.1.4
                                rust-ppv-lite86-0.2.21
                                rust-presser-0.3.1
                                rust-proc-macro-crate-3.4.0
                                rust-proc-macro2-1.0.103
                                rust-proc-macro2-diagnostics-0.10.1
                                rust-profiling-1.0.17
                                rust-qoi-0.4.1
                                rust-quick-xml-0.37.5
                                rust-quinn-0.11.9
                                rust-quinn-proto-0.11.13
                                rust-quinn-udp-0.5.14
                                rust-quote-1.0.41
                                rust-r-efi-5.3.0
                                rust-rand-0.8.5
                                rust-rand-0.9.2
                                rust-rand-chacha-0.3.1
                                rust-rand-chacha-0.9.0
                                rust-rand-core-0.6.4
                                rust-rand-core-0.9.3
                                rust-range-alloc-0.1.4
                                rust-rangemap-1.6.0
                                rust-raw-window-handle-0.6.2
                                rust-rayon-1.11.0
                                rust-rayon-core-1.13.0
                                rust-read-fonts-0.22.7
                                rust-redox-syscall-0.2.16
                                rust-redox-syscall-0.4.1
                                rust-redox-syscall-0.5.18
                                rust-redox-users-0.4.6
                                rust-regex-1.12.2
                                rust-regex-automata-0.4.13
                                rust-regex-syntax-0.8.8
                                rust-relative-path-1.9.3
                                rust-renderdoc-sys-1.1.0
                                rust-reqwest-0.12.24
                                rust-resvg-0.42.0
                                rust-rfd-0.15.4
                                rust-rgb-0.8.52
                                rust-ring-0.17.14
                                rust-rodio-0.21.1
                                rust-roxmltree-0.20.0
                                rust-rstest-0.26.1
                                rust-rstest-macros-0.26.1
                                rust-rust-ini-0.18.0
                                rust-rustc-hash-1.1.0
                                rust-rustc-hash-2.1.1
                                rust-rustc-version-0.4.1
                                rust-rustix-0.38.44
                                rust-rustix-1.1.2
                                rust-rustls-0.23.34
                                rust-rustls-pki-types-1.13.0
                                rust-rustls-webpki-0.103.8
                                rust-rustrict-0.7.36
                                rust-rustversion-1.0.22
                                rust-rustybuzz-0.14.1
                                rust-ryu-1.0.20
                                rust-same-file-1.0.6
                                rust-scc-2.4.0
                                rust-schannel-0.1.28
                                rust-scoped-tls-1.0.1
                                rust-scopeguard-1.2.0
                                rust-sctk-adwaita-0.10.1
                                rust-sdd-3.0.10
                                rust-security-framework-2.11.1
                                rust-security-framework-sys-2.15.0
                                rust-self-cell-1.2.1
                                rust-semver-1.0.27
                                rust-serde-1.0.228
                                rust-serde-core-1.0.228
                                rust-serde-derive-1.0.228
                                rust-serde-json-1.0.145
                                rust-serde-repr-0.1.20
                                rust-serde-spanned-1.0.3
                                rust-serde-test-1.0.177
                                rust-serde-urlencoded-0.7.1
                                rust-serial-test-3.2.0
                                rust-serial-test-derive-3.2.0
                                rust-sha1-0.10.6
                                rust-shlex-1.3.0
                                rust-signal-hook-registry-1.4.6
                                rust-simd-adler32-0.3.7
                                rust-simplecss-0.2.2
                                rust-siphasher-1.0.1
                                rust-skrifa-0.22.3
                                rust-slab-0.4.11
                                rust-slotmap-1.0.7
                                rust-smallvec-1.15.1
                                rust-smithay-client-toolkit-0.19.2
                                rust-smithay-clipboard-0.7.2
                                rust-smol-str-0.2.2
                                rust-socket2-0.6.1
                                rust-softbuffer-0.4.6
                                rust-spirv-0.3.0+sdk-1.3.268.0
                                rust-splines-5.0.0
                                rust-stable-deref-trait-1.2.1
                                rust-static-assertions-1.1.0
                                rust-strict-num-0.1.1
                                rust-strsim-0.11.1
                                rust-subtle-2.6.1
                                rust-svg-fmt-0.4.5
                                rust-svgtypes-0.15.3
                                rust-swash-0.1.19
                                rust-symphonia-0.5.5
                                rust-symphonia-bundle-mp3-0.5.5
                                rust-symphonia-core-0.5.5
                                rust-symphonia-metadata-0.5.5
                                rust-syn-1.0.109
                                rust-syn-2.0.108
                                rust-sync-wrapper-1.0.2
                                rust-synstructure-0.13.2
                                rust-sys-locale-0.3.2
                                rust-system-configuration-0.6.1
                                rust-system-configuration-sys-0.6.0
                                rust-tempfile-3.23.0
                                rust-termcolor-1.4.1
                                rust-thiserror-1.0.69
                                rust-thiserror-2.0.17
                                rust-thiserror-impl-1.0.69
                                rust-thiserror-impl-2.0.17
                                rust-tiff-0.9.1
                                rust-tiny-skia-0.11.4
                                rust-tiny-skia-path-0.11.4
                                rust-tiny-xlib-0.2.4
                                rust-tinystr-0.8.2
                                rust-tinyvec-1.10.0
                                rust-tinyvec-macros-0.1.1
                                rust-tokio-1.48.0
                                rust-tokio-macros-2.6.0
                                rust-tokio-native-tls-0.3.1
                                rust-tokio-rustls-0.26.4
                                rust-tokio-util-0.7.17
                                rust-toml-0.5.11
                                rust-toml-0.9.8
                                rust-toml-datetime-0.7.3
                                rust-toml-edit-0.23.7
                                rust-toml-parser-1.0.4
                                rust-toml-writer-1.0.4
                                rust-tower-0.5.2
                                rust-tower-http-0.6.6
                                rust-tower-layer-0.3.3
                                rust-tower-service-0.3.3
                                rust-tracing-0.1.41
                                rust-tracing-attributes-0.1.30
                                rust-tracing-core-0.1.34
                                rust-try-lock-0.2.5
                                rust-ttf-parser-0.20.0
                                rust-ttf-parser-0.21.1
                                rust-ttf-parser-0.25.1
                                rust-typenum-1.19.0
                                rust-uds-windows-1.1.0
                                rust-unicode-bidi-0.3.18
                                rust-unicode-bidi-mirroring-0.2.0
                                rust-unicode-ccc-0.2.0
                                rust-unicode-ident-1.0.22
                                rust-unicode-linebreak-0.1.5
                                rust-unicode-normalization-0.1.25
                                rust-unicode-properties-0.1.4
                                rust-unicode-script-0.5.7
                                rust-unicode-segmentation-1.12.0
                                rust-unicode-vo-0.1.0
                                rust-unicode-width-0.1.14
                                rust-unicode-xid-0.2.6
                                rust-untrusted-0.9.0
                                rust-url-2.5.7
                                rust-urlencoding-2.1.3
                                rust-usvg-0.42.0
                                rust-utf8-iter-1.0.4
                                rust-utf8parse-0.2.2
                                rust-uuid-1.18.1
                                rust-vcpkg-0.2.15
                                rust-version-check-0.9.5
                                rust-walkdir-2.5.0
                                rust-want-0.3.1
                                rust-wasi-0.11.1+wasi-snapshot-preview1
                                rust-wasip2-1.0.1+wasi-0.2.4
                                rust-wasm-bindgen-0.2.105
                                rust-wasm-bindgen-futures-0.4.55
                                rust-wasm-bindgen-macro-0.2.105
                                rust-wasm-bindgen-macro-support-0.2.105
                                rust-wasm-bindgen-shared-0.2.105
                                rust-wasm-timer-0.2.5
                                rust-wayland-backend-0.3.11
                                rust-wayland-client-0.31.11
                                rust-wayland-csd-frame-0.3.0
                                rust-wayland-cursor-0.31.11
                                rust-wayland-protocols-0.32.9
                                rust-wayland-protocols-plasma-0.3.9
                                rust-wayland-protocols-wlr-0.3.9
                                rust-wayland-scanner-0.31.7
                                rust-wayland-sys-0.31.7
                                rust-web-sys-0.3.82
                                rust-web-time-1.1.0
                                rust-webpki-roots-1.0.4
                                rust-weezl-0.1.10
                                rust-wgpu-0.19.4
                                rust-wgpu-core-0.19.4
                                rust-wgpu-hal-0.19.5
                                rust-wgpu-types-0.19.2
                                rust-widestring-1.2.1
                                rust-winapi-0.3.9
                                rust-winapi-i686-pc-windows-gnu-0.4.0
                                rust-winapi-util-0.1.11
                                rust-winapi-x86-64-pc-windows-gnu-0.4.0
                                rust-window-clipboard-0.4.1
                                rust-windows-0.52.0
                                rust-windows-0.54.0
                                rust-windows-core-0.52.0
                                rust-windows-core-0.54.0
                                rust-windows-core-0.62.2
                                rust-windows-implement-0.60.2
                                rust-windows-interface-0.59.3
                                rust-windows-link-0.1.3
                                rust-windows-link-0.2.1
                                rust-windows-registry-0.5.3
                                rust-windows-result-0.1.2
                                rust-windows-result-0.3.4
                                rust-windows-result-0.4.1
                                rust-windows-strings-0.4.2
                                rust-windows-strings-0.5.1
                                rust-windows-sys-0.36.1
                                rust-windows-sys-0.45.0
                                rust-windows-sys-0.52.0
                                rust-windows-sys-0.59.0
                                rust-windows-sys-0.60.2
                                rust-windows-sys-0.61.2
                                rust-windows-targets-0.42.2
                                rust-windows-targets-0.52.6
                                rust-windows-targets-0.53.5
                                rust-windows-aarch64-gnullvm-0.42.2
                                rust-windows-aarch64-gnullvm-0.52.6
                                rust-windows-aarch64-gnullvm-0.53.1
                                rust-windows-aarch64-msvc-0.36.1
                                rust-windows-aarch64-msvc-0.42.2
                                rust-windows-aarch64-msvc-0.52.6
                                rust-windows-aarch64-msvc-0.53.1
                                rust-windows-i686-gnu-0.36.1
                                rust-windows-i686-gnu-0.42.2
                                rust-windows-i686-gnu-0.52.6
                                rust-windows-i686-gnu-0.53.1
                                rust-windows-i686-gnullvm-0.52.6
                                rust-windows-i686-gnullvm-0.53.1
                                rust-windows-i686-msvc-0.36.1
                                rust-windows-i686-msvc-0.42.2
                                rust-windows-i686-msvc-0.52.6
                                rust-windows-i686-msvc-0.53.1
                                rust-windows-x86-64-gnu-0.36.1
                                rust-windows-x86-64-gnu-0.42.2
                                rust-windows-x86-64-gnu-0.52.6
                                rust-windows-x86-64-gnu-0.53.1
                                rust-windows-x86-64-gnullvm-0.42.2
                                rust-windows-x86-64-gnullvm-0.52.6
                                rust-windows-x86-64-gnullvm-0.53.1
                                rust-windows-x86-64-msvc-0.36.1
                                rust-windows-x86-64-msvc-0.42.2
                                rust-windows-x86-64-msvc-0.52.6
                                rust-windows-x86-64-msvc-0.53.1
                                rust-winit-0.30.12
                                rust-winnow-0.7.13
                                rust-winreg-0.10.1
                                rust-winres-0.1.12
                                rust-wit-bindgen-0.46.0
                                rust-writeable-0.6.2
                                rust-x11-dl-2.21.0
                                rust-x11rb-0.13.2
                                rust-x11rb-protocol-0.13.2
                                rust-xcursor-0.3.10
                                rust-xdg-home-1.3.0
                                rust-xkbcommon-dl-0.4.2
                                rust-xkeysym-0.2.1
                                rust-xml-rs-0.8.28
                                rust-xmlwriter-0.1.0
                                rust-yansi-1.0.1
                                rust-yazi-0.1.6
                                rust-yoke-0.8.1
                                rust-yoke-derive-0.8.1
                                rust-zbus-4.4.0
                                rust-zbus-5.12.0
                                rust-zbus-macros-4.4.0
                                rust-zbus-macros-5.12.0
                                rust-zbus-names-3.0.0
                                rust-zbus-names-4.2.0
                                rust-zeno-0.2.3
                                rust-zerocopy-0.8.27
                                rust-zerocopy-derive-0.8.27
                                rust-zerofrom-0.1.6
                                rust-zerofrom-derive-0.1.6
                                rust-zeroize-1.8.2
                                rust-zerotrie-0.2.3
                                rust-zerovec-0.11.5
                                rust-zerovec-derive-0.11.2
                                rust-zune-inflate-0.2.54
                                rust-zvariant-4.2.0
                                rust-zvariant-5.8.0
                                rust-zvariant-derive-4.2.0
                                rust-zvariant-derive-5.8.0
                                rust-zvariant-utils-2.1.0
                                rust-zvariant-utils-3.2.1))
                     (wakatime-ls =>
                                  (list rust-adler2-2.0.0
                                   rust-android-system-properties-0.1.5
                                   rust-anstream-0.6.15
                                   rust-anstyle-1.0.8
                                   rust-anstyle-parse-0.2.5
                                   rust-anstyle-query-1.1.1
                                   rust-anstyle-wincon-3.0.4
                                   rust-anyhow-1.0.86
                                   rust-arc-swap-1.7.1
                                   rust-async-trait-0.1.81
                                   rust-auditable-serde-0.8.0
                                   rust-auto-impl-1.2.0
                                   rust-autocfg-1.3.0
                                   rust-bitflags-1.3.2
                                   rust-bitflags-2.6.0
                                   rust-bumpalo-3.16.0
                                   rust-bytes-1.7.1
                                   rust-cc-1.1.15
                                   rust-cfg-if-1.0.0
                                   rust-chrono-0.4.42
                                   rust-clap-4.5.49
                                   rust-clap-builder-4.5.49
                                   rust-clap-lex-0.7.4
                                   rust-colorchoice-1.0.2
                                   rust-core-foundation-sys-0.8.7
                                   rust-crc32fast-1.4.2
                                   rust-dashmap-5.5.3
                                   rust-displaydoc-0.2.5
                                   rust-equivalent-1.0.1
                                   rust-flate2-1.1.1
                                   rust-foldhash-0.1.5
                                   rust-form-urlencoded-1.2.2
                                   rust-futures-0.3.30
                                   rust-futures-channel-0.3.30
                                   rust-futures-core-0.3.30
                                   rust-futures-executor-0.3.30
                                   rust-futures-io-0.3.30
                                   rust-futures-macro-0.3.30
                                   rust-futures-sink-0.3.30
                                   rust-futures-task-0.3.30
                                   rust-futures-util-0.3.30
                                   rust-hashbrown-0.14.5
                                   rust-hashbrown-0.15.2
                                   rust-heck-0.5.0
                                   rust-hermit-abi-0.3.9
                                   rust-httparse-1.9.4
                                   rust-iana-time-zone-0.1.60
                                   rust-iana-time-zone-haiku-0.1.2
                                   rust-icu-collections-1.5.0
                                   rust-icu-locid-1.5.0
                                   rust-icu-locid-transform-1.5.0
                                   rust-icu-locid-transform-data-1.5.0
                                   rust-icu-normalizer-1.5.0
                                   rust-icu-normalizer-data-1.5.0
                                   rust-icu-properties-1.5.1
                                   rust-icu-properties-data-1.5.0
                                   rust-icu-provider-1.5.0
                                   rust-icu-provider-macros-1.5.0
                                   rust-id-arena-2.2.1
                                   rust-idna-1.1.0
                                   rust-idna-adapter-1.2.0
                                   rust-indexmap-2.9.0
                                   rust-is-terminal-polyfill-1.70.1
                                   rust-itoa-1.0.11
                                   rust-js-sys-0.3.70
                                   rust-leb128fmt-0.1.0
                                   rust-libc-0.2.174
                                   rust-litemap-0.7.5
                                   rust-lock-api-0.4.12
                                   rust-log-0.4.22
                                   rust-lsp-types-0.94.1
                                   rust-memchr-2.7.4
                                   rust-miniz-oxide-0.8.8
                                   rust-mio-1.0.2
                                   rust-num-traits-0.2.19
                                   rust-once-cell-1.19.0
                                   rust-parking-lot-core-0.9.10
                                   rust-percent-encoding-2.3.2
                                   rust-pin-project-1.1.5
                                   rust-pin-project-internal-1.1.5
                                   rust-pin-project-lite-0.2.14
                                   rust-pin-utils-0.1.0
                                   rust-prettyplease-0.2.32
                                   rust-proc-macro2-1.0.93
                                   rust-quote-1.0.36
                                   rust-redox-syscall-0.5.3
                                   rust-ryu-1.0.18
                                   rust-scopeguard-1.2.0
                                   rust-semver-1.0.23
                                   rust-serde-1.0.228
                                   rust-serde-core-1.0.228
                                   rust-serde-derive-1.0.228
                                   rust-serde-json-1.0.145
                                   rust-serde-repr-0.1.19
                                   rust-shlex-1.3.0
                                   rust-signal-hook-registry-1.4.5
                                   rust-slab-0.4.9
                                   rust-smallvec-1.13.2
                                   rust-spdx-0.10.6
                                   rust-stable-deref-trait-1.2.0
                                   rust-strsim-0.11.1
                                   rust-syn-2.0.98
                                   rust-synstructure-0.13.1
                                   rust-tinystr-0.7.6
                                   rust-tokio-1.48.0
                                   rust-tokio-macros-2.6.0
                                   rust-tokio-util-0.7.11
                                   rust-topological-sort-0.2.2
                                   rust-tower-0.4.13
                                   rust-tower-layer-0.3.3
                                   rust-tower-lsp-0.20.0
                                   rust-tower-lsp-macros-0.9.0
                                   rust-tower-service-0.3.3
                                   rust-tracing-0.1.40
                                   rust-tracing-attributes-0.1.27
                                   rust-tracing-core-0.1.32
                                   rust-unicode-ident-1.0.12
                                   rust-unicode-xid-0.2.4
                                   rust-url-2.5.7
                                   rust-utf16-iter-1.0.5
                                   rust-utf8-iter-1.0.4
                                   rust-utf8parse-0.2.2
                                   rust-wasi-0.11.0+wasi-snapshot-preview1
                                   rust-wasm-bindgen-0.2.93
                                   rust-wasm-bindgen-backend-0.2.93
                                   rust-wasm-bindgen-macro-0.2.93
                                   rust-wasm-bindgen-macro-support-0.2.93
                                   rust-wasm-bindgen-shared-0.2.93
                                   rust-wasm-encoder-0.227.1
                                   rust-wasm-metadata-0.227.1
                                   rust-wasmparser-0.227.1
                                   rust-windows-core-0.52.0
                                   rust-windows-link-0.2.0
                                   rust-windows-sys-0.52.0
                                   rust-windows-sys-0.61.1
                                   rust-windows-targets-0.52.6
                                   rust-windows-aarch64-gnullvm-0.52.6
                                   rust-windows-aarch64-msvc-0.52.6
                                   rust-windows-i686-gnu-0.52.6
                                   rust-windows-i686-gnullvm-0.52.6
                                   rust-windows-i686-msvc-0.52.6
                                   rust-windows-x86-64-gnu-0.52.6
                                   rust-windows-x86-64-gnullvm-0.52.6
                                   rust-windows-x86-64-msvc-0.52.6
                                   rust-wit-bindgen-0.41.0
                                   rust-wit-bindgen-core-0.41.0
                                   rust-wit-bindgen-rt-0.41.0
                                   rust-wit-bindgen-rust-0.41.0
                                   rust-wit-bindgen-rust-macro-0.41.0
                                   rust-wit-component-0.227.1
                                   rust-wit-parser-0.227.1
                                   rust-write16-1.0.0
                                   rust-writeable-0.5.5
                                   rust-yoke-0.7.5
                                   rust-yoke-derive-0.7.5
                                   rust-zed-extension-api-0.7.0
                                   rust-zerofrom-0.1.6
                                   rust-zerofrom-derive-0.1.6
                                   rust-zerovec-0.10.4
                                   rust-zerovec-derive-0.10.3))
                     (wluma =>
                            (list rust-addr2line-0.24.2
                                  rust-adler2-2.0.0
                                  rust-aho-corasick-1.1.3
                                  rust-android-tzdata-0.1.1
                                  rust-android-system-properties-0.1.5
                                  rust-anstream-0.6.18
                                  rust-anstyle-1.0.10
                                  rust-anstyle-parse-0.2.6
                                  rust-anstyle-query-1.1.2
                                  rust-anstyle-wincon-3.0.7
                                  rust-anyhow-1.0.96
                                  rust-ash-0.38.0+1.3.281
                                  rust-async-channel-2.3.1
                                  rust-async-executor-1.13.2
                                  rust-async-fs-2.1.2
                                  rust-async-io-2.4.1
                                  rust-async-lock-3.4.0
                                  rust-async-net-2.0.0
                                  rust-async-process-2.3.1
                                  rust-async-signal-0.2.11
                                  rust-async-task-4.7.1
                                  rust-atomic-waker-1.1.2
                                  rust-autocfg-1.4.0
                                  rust-backtrace-0.3.74
                                  rust-bindgen-0.65.1
                                  rust-bitflags-1.3.2
                                  rust-bitflags-2.8.0
                                  rust-blocking-1.6.1
                                  rust-bumpalo-3.17.0
                                  rust-byteorder-1.5.0
                                  rust-cc-1.2.15
                                  rust-cexpr-0.6.0
                                  rust-cfg-if-1.0.0
                                  rust-chrono-0.4.40
                                  rust-clang-sys-1.8.1
                                  rust-colorchoice-1.0.3
                                  rust-concurrent-queue-2.5.0
                                  rust-core-foundation-0.10.0
                                  rust-core-foundation-sys-0.8.7
                                  rust-core-graphics-0.24.0
                                  rust-core-graphics-types-0.2.0
                                  rust-crossbeam-utils-0.8.21
                                  rust-dbus-0.9.7
                                  rust-ddc-0.2.2
                                  rust-ddc-hi-0.4.1
                                  rust-ddc-i2c-0.2.2
                                  rust-ddc-macos-0.2.2
                                  rust-ddc-winapi-0.2.2
                                  rust-downcast-rs-1.2.1
                                  rust-drm-fourcc-2.2.0
                                  rust-dtoa-0.4.8
                                  rust-edid-0.3.0
                                  rust-either-1.14.0
                                  rust-env-filter-0.1.3
                                  rust-env-logger-0.11.6
                                  rust-equivalent-1.0.2
                                  rust-errno-0.3.10
                                  rust-event-listener-5.4.0
                                  rust-event-listener-strategy-0.5.4
                                  rust-fastrand-2.3.0
                                  rust-foreign-types-0.5.0
                                  rust-foreign-types-macros-0.2.3
                                  rust-foreign-types-shared-0.3.1
                                  rust-futures-core-0.3.31
                                  rust-futures-io-0.3.31
                                  rust-futures-lite-2.6.0
                                  rust-futures-macro-0.3.31
                                  rust-futures-task-0.3.31
                                  rust-futures-util-0.3.31
                                  rust-gimli-0.31.1
                                  rust-glob-0.3.2
                                  rust-hashbrown-0.15.2
                                  rust-hermit-abi-0.5.1
                                  rust-home-0.5.11
                                  rust-humantime-2.1.0
                                  rust-i2c-0.1.0
                                  rust-i2c-linux-0.1.2
                                  rust-i2c-linux-sys-0.2.1
                                  rust-iana-time-zone-0.1.61
                                  rust-iana-time-zone-haiku-0.1.2
                                  rust-indexmap-2.7.1
                                  rust-inotify-0.11.0
                                  rust-inotify-sys-0.1.5
                                  rust-io-kit-sys-0.4.1
                                  rust-is-terminal-polyfill-1.70.1
                                  rust-itertools-0.14.0
                                  rust-itoa-1.0.14
                                  rust-js-sys-0.3.77
                                  rust-lazy-static-1.5.0
                                  rust-lazycell-1.3.0
                                  rust-libc-0.2.170
                                  rust-libdbus-sys-0.2.5
                                  rust-libloading-0.8.6
                                  rust-libudev-sys-0.1.4
                                  rust-linked-hash-map-0.5.6
                                  rust-linux-raw-sys-0.4.15
                                  rust-linux-raw-sys-0.9.4
                                  rust-log-0.4.26
                                  rust-mach2-0.4.2
                                  rust-macro-rules-attribute-0.2.0
                                  rust-macro-rules-attribute-proc-macro-0.2.0
                                  rust-mccs-0.1.3
                                  rust-mccs-caps-0.1.3
                                  rust-mccs-db-0.1.3
                                  rust-memchr-1.0.2
                                  rust-memchr-2.7.4
                                  rust-minimal-lexical-0.2.1
                                  rust-miniz-oxide-0.8.5
                                  rust-mio-1.0.3
                                  rust-nom-3.2.1
                                  rust-nom-7.1.3
                                  rust-num-traits-0.2.19
                                  rust-nvapi-0.1.4
                                  rust-nvapi-sys-0.1.3
                                  rust-object-0.36.7
                                  rust-once-cell-1.20.3
                                  rust-parking-2.2.1
                                  rust-paste-1.0.15
                                  rust-peeking-take-while-0.1.2
                                  rust-pin-project-lite-0.2.16
                                  rust-pin-utils-0.1.0
                                  rust-piper-0.2.4
                                  rust-pkg-config-0.3.31
                                  rust-polling-3.8.0
                                  rust-prettyplease-0.2.29
                                  rust-proc-macro2-1.0.93
                                  rust-quick-xml-0.37.2
                                  rust-quote-1.0.38
                                  rust-regex-1.11.1
                                  rust-regex-automata-0.4.9
                                  rust-regex-syntax-0.8.5
                                  rust-resize-slice-0.1.3
                                  rust-rustc-demangle-0.1.24
                                  rust-rustc-hash-1.1.0
                                  rust-rustix-0.38.44
                                  rust-rustix-1.0.7
                                  rust-rustversion-1.0.19
                                  rust-ryu-1.0.19
                                  rust-serde-1.0.218
                                  rust-serde-derive-1.0.218
                                  rust-serde-spanned-0.6.8
                                  rust-serde-yaml-0.7.5
                                  rust-serde-yaml-0.9.34+deprecated
                                  rust-shlex-1.3.0
                                  rust-signal-hook-registry-1.4.5
                                  rust-slab-0.4.9
                                  rust-smallvec-1.14.0
                                  rust-smol-2.0.2
                                  rust-smol-macros-0.1.1
                                  rust-socket2-0.5.8
                                  rust-syn-2.0.98
                                  rust-thiserror-1.0.69
                                  rust-thiserror-impl-1.0.69
                                  rust-tokio-1.43.0
                                  rust-toml-0.8.20
                                  rust-toml-datetime-0.6.8
                                  rust-toml-edit-0.22.24
                                  rust-tracing-0.1.41
                                  rust-tracing-core-0.1.33
                                  rust-udev-0.2.0
                                  rust-unicode-ident-1.0.17
                                  rust-uninitialized-0.0.2
                                  rust-unsafe-libyaml-0.2.11
                                  rust-utf8parse-0.2.2
                                  rust-v4l-0.14.0
                                  rust-v4l-sys-0.3.0
                                  rust-void-1.0.2
                                  rust-wasi-0.11.0+wasi-snapshot-preview1
                                  rust-wasm-bindgen-0.2.100
                                  rust-wasm-bindgen-backend-0.2.100
                                  rust-wasm-bindgen-macro-0.2.100
                                  rust-wasm-bindgen-macro-support-0.2.100
                                  rust-wasm-bindgen-shared-0.2.100
                                  rust-wayland-backend-0.3.8
                                  rust-wayland-client-0.31.8
                                  rust-wayland-protocols-0.32.6
                                  rust-wayland-protocols-wlr-0.3.6
                                  rust-wayland-scanner-0.31.6
                                  rust-wayland-sys-0.31.6
                                  rust-which-4.4.2
                                  rust-widestring-1.1.0
                                  rust-winapi-0.3.9
                                  rust-winapi-i686-pc-windows-gnu-0.4.0
                                  rust-winapi-x86-64-pc-windows-gnu-0.4.0
                                  rust-windows-core-0.52.0
                                  rust-windows-link-0.1.0
                                  rust-windows-sys-0.52.0
                                  rust-windows-sys-0.59.0
                                  rust-windows-targets-0.52.6
                                  rust-windows-aarch64-gnullvm-0.52.6
                                  rust-windows-aarch64-msvc-0.52.6
                                  rust-windows-i686-gnu-0.52.6
                                  rust-windows-i686-gnullvm-0.52.6
                                  rust-windows-i686-msvc-0.52.6
                                  rust-windows-x86-64-gnu-0.52.6
                                  rust-windows-x86-64-gnullvm-0.52.6
                                  rust-windows-x86-64-msvc-0.52.6
                                  rust-winnow-0.7.3
                                  rust-xdg-2.5.2
                                  rust-yaml-rust-0.4.5)))
