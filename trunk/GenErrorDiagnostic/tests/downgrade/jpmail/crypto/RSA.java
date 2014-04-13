package crypto;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.security.spec.InvalidKeySpecException;
import java.security.spec.PKCS8EncodedKeySpec;
import java.security.spec.X509EncodedKeySpec;
import java.security.InvalidKeyException;
import java.security.KeyFactory;
import java.security.KeyPairGenerator;
import java.security.KeyPair;
import java.security.NoSuchAlgorithmException;
import java.security.InvalidAlgorithmParameterException;
import java.security.PrivateKey;
import java.security.PublicKey;
import java.security.Key;
import javax.crypto.BadPaddingException;
import javax.crypto.Cipher;
import javax.crypto.IllegalBlockSizeException;
import javax.crypto.KeyGenerator;
import javax.crypto.NoSuchPaddingException;
import org.bouncycastle.jce.provider.BouncyCastleProvider;
import java.io.PrintStream;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.FileOutputStream;
import java.io.FileInputStream;

public class RSA {
    
    public static KeyPair generateKeyPair(final jif.lang.Label jif$L)
          throws NoSuchAlgorithmException {
        KeyPair keyPair = null;
        try {
            keyPair =
              KeyPairGenerator.getInstance(
                "RSA", new BouncyCastleProvider()).generateKeyPair();
        }
        catch (final NullPointerException ignore) {  }
        return keyPair;
    }
    
    public static PublicKey getPublicKey(final jif.lang.Label jif$L,
                                         final InputStream in,
                                         final jif.lang.Label lbl) {
        final ByteArrayOutputStream pubKeyBaos = new ByteArrayOutputStream();
        X509EncodedKeySpec pubKeySpec = null;
        int curByte = 0;
        if (pubKeyBaos != null && in != null) {
            try {
                while ((curByte = in.read()) != -1) {
                    pubKeyBaos.write(curByte);
                }
            }
            catch (final IOException e) {  }
            pubKeySpec = new X509EncodedKeySpec(pubKeyBaos.toByteArray());
            try {
                pubKeyBaos.close();
            }
            catch (final IOException e) {  }
        }
        PublicKey pubKey = null;
        try {
            pubKey =
              KeyFactory.getInstance("RSA",
                                     new BouncyCastleProvider()).generatePublic(
                pubKeySpec);
        }
        catch (final InvalidKeySpecException e) {  }
        catch (final NoSuchAlgorithmException e) {  }
        catch (final NullPointerException ignore) {  }
        return pubKey;
    }
    
    public static PrivateKey getPrivateKey(final jif.lang.Label jif$L,
                                           final InputStream in,
                                           final jif.lang.Label lbl) {
        final ByteArrayOutputStream privKeyBaos = new ByteArrayOutputStream();
        int curByte = 0;
        PKCS8EncodedKeySpec privKeySpec = null;
        if (privKeyBaos != null && in != null) {
            try {
                while ((curByte = in.read()) != -1) {
                    privKeyBaos.write(curByte);
                }
            }
            catch (final IOException e) {  }
            privKeySpec = new PKCS8EncodedKeySpec(privKeyBaos.toByteArray());
            try {
                privKeyBaos.close();
            }
            catch (final IOException e) {  }
        }
        PrivateKey privKey = null;
        try {
            privKey =
              KeyFactory.getInstance(
                "RSA", new BouncyCastleProvider()).generatePrivate(
                privKeySpec);
        }
        catch (final InvalidKeySpecException e) {  }
        catch (final NoSuchAlgorithmException e) {  }
        catch (final NullPointerException ignore) {  }
        return privKey;
    }
    
    public static Ciphertext encrypt(final jif.lang.Label jif$L, final Key key,
                                     final String s) {
        Ciphertext ciphertext = null;
        try {
            final Cipher rsaCipher_ =
              Cipher.getInstance("RSA/ECB/PKCS1Padding",
                                 new BouncyCastleProvider());
            if (rsaCipher_ != null) {
                rsaCipher_.init(Cipher.ENCRYPT_MODE, key);
                final byte[] input = s.getBytes();
                final byte[] encrypted = rsaCipher_.doFinal(input);
                ciphertext = new Ciphertext().crypto$Ciphertext$(encrypted);
            }
        }
        catch (final Exception e) {  }
        return ciphertext;
    }
    
    public static Ciphertext encrypt(final jif.lang.Label jif$L, final Key key,
                                     final byte[] input) {
        Ciphertext ciphertext = null;
        try {
            final Cipher rsaCipher_ =
              Cipher.getInstance("RSA/ECB/PKCS1Padding",
                                 new BouncyCastleProvider());
            if (rsaCipher_ != null) {
                rsaCipher_.init(Cipher.ENCRYPT_MODE, key);
                final byte[] encrypted = rsaCipher_.doFinal(input);
                ciphertext = new Ciphertext().crypto$Ciphertext$(encrypted);
            }
        }
        catch (final Exception e) {  }
        return ciphertext;
    }
    
    public static String decrypt(final jif.lang.Label jif$L, final Key key,
                                 final Ciphertext ciph)
          throws InvalidKeyException,
        IllegalBlockSizeException,
        BadPaddingException,
        NoSuchPaddingException,
        InvalidAlgorithmParameterException,
        NoSuchAlgorithmException,
        NullPointerException,
        IllegalArgumentException {
        Cipher rsaCipher_ =
          Cipher.getInstance("RSA/ECB/PKCS1Padding",
                             new BouncyCastleProvider());
        rsaCipher_.init(Cipher.DECRYPT_MODE, key);
        String output = new String(rsaCipher_.doFinal(ciph.encText));
        return output;
    }
    
    public RSA crypto$RSA$() {
        this.jif$init();
        {  }
        return this;
    }
    
    final public static String jlc$CompilerVersion$jif = "2.0.1";
    final public static long jlc$SourceLastModified$jif = 1396370676000L;
    final public static String jlc$ClassType$jif =
      ("H4sIAAAAAAAAAL1be3AcxZnvXVlPy5EtC8s2eqxlS35g7zrBAYKcgCTLRvba" +
       "CEv2GREsRrMtadDs\nznimV16Zsg+MD/tICAFixyTBFxLyKEOoAxfJxUUSE5" +
       "t3hZyTCuTuMAdUjoMLTuouoUKqkuK+r3tm\n57EzkmzL+aNbsz399ffo7/v1" +
       "1z2tx86SYtMg9bcqg3E2plMzvl4Z7JYMk6a6NXWsF5r65V/cdeTh\nZ6969/" +
       "koKUqScinLhjVDYWOMzEzeKo1KiSxT1ERSMVlrksxUMiaTMkyRGE2tNbQ0Iw" +
       "uSOgw1pGos\nQXMsoUuGlE5wZonuDlUyTSAr4a3mDrKHRHIGWWxTxIEifqtq" +
       "ySYE4zRCsnt+t780Ed0SKSJVfaRK\nyfQwiSlyh5ZhQNdHKtM0PUANsy2Voq" +
       "k+MitDaaqHGoqkKrugo5bpI9WmMpSRWNag5mZqauoodqw2\nszo1OE+7MUkq" +
       "ZQ1UM7Iy0wwhKIg9qFA1Zf8qHlSlIZOROY7CQs212A5aViggmDEoydQmmTai" +
       "ZFKM\nNPop8jou2gAdgLQ0TcHseVbTMhI0kGoxAaqUGUr0MEPJDEHXYi0LXB" +
       "iZHzoodCrTJXlEGqL9jMz1\n9+sWr6BXOTcEkjByib8bHwkma0HwZLmm6d50" +
       "0ZfvOvFebZSLnqKyimqUAm1zMO1mOkgNmpGpoL9m\nd2n5u2+va48SAjR1wT" +
       "Si65v9Jy75111/3iu6NoV3vX7gViqzfvnM8aazFW3LdxehbGW6ZiroGB6r\n" +
       "cP/utt605nQImDn5gfFl3H55YvNzN95+lP42Skq6SImsqdl0pouU00yqw3ou" +
       "heekkqFdZJoKf8Ac\ng4pK0Rwl8KxLbJg/53RCSCmUmVCWYGGkdHNPWxwClZ" +
       "GPmVrWkGnCasghSdXOSAQEWxgSOSo43XWa\nmqJGv7yyceiHP1r7enM070kW" +
       "T0YqZGNMZ1ocRiaRCB+x1qsq2i6Fwf7+k60zv7DC/D6gQh8pV9Lp\nLJMGVN" +
       "CoUlJVbSdN9TPuN7NcPmqHe+UAuBh4a78KA4mg18moe74sH3JCrovjikxvu2" +
       "ITOdjQ+RWc\nYJyJGhxdiAZ2HRGyVS7tuXn9LQeairDTzmlgP9SkyYNyAWP3" +
       "y99/+Gl25mTzH6KkuI/UKOYaOihl\nVbaZAkJkktIAVfvI7HwzwI3BeGuSTB" +
       "fwIEGI20E63XDIGKlNAnsrdFRsSvAXCAtmfhykbATHvSwk\npoJkfv/p1neS" +
       "K9/uFS6/NMQBDE2mKYA5h+7Odb/v/80DDX2gK0Q6iM9AeASOBn+ke8Kx1Qph" +
       "RpoK\ngMPPpNUGRdRrGlhkUDPSkorD2EaqYMOGttNp4c48A6tZwq+xWoQT7f" +
       "MNDqp/6Np/3TsvL7k56sbf\nKtey1EOZiNhZjp/0GpRC+5nD3Q8cPLv/Ju4k" +
       "lpcwWImyA6oi57g0tRFwytk+xihrfG7Nlw4t/dpr\nthfOdkZvMwxpDJ0wd8" +
       "cv6h98XnoIkAWi3VR2UR7VhHMiNgOsV/DnuOslRJ7D33HbNtOECQKgWV17\n" +
       "4O2f17/SK/j7qUGgSx0i7mzx9ZoiPLFfvqrr7dO5J0e+zo1WIWtpXctQy21b" +
       "wIVq/bQW3T9/480T\n9NTyG6MkClOZoqZsKDpKY8FXCdPWgxlwOeOubkgZU4" +
       "U8QDh6L3/ZmdONVvccM9LarjGmpWPaYIwN\n0xhnCTWDlZwu501pzWQxMS/W" +
       "a8BpUwG0QcTj0tq8HD0dfv3yy7+695WzHx6/U4RIo5eioPc7Lz01\nsn3bT8" +
       "7ak9vsTC7AuArLBuhsLtqSSWspZVBB1AN3ur3+/tP3fvX2LYLH8olpnPZ57e" +
       "T2l7f/qYFP\nSETGDMLxYKebcORaP+BdJ5nDwP/X6mt9B88saxD8Xe5uvT++" +
       "5h8OHvqXH6wSmFiJjnLNtYTYDjPf\nP+m9mm7N+1t3XP54b6aVOxtO2pX2zH" +
       "0K+kw4bYYCLkEL563ZOwsdUiajsYK5+J/rf3dil05fsHmv\ntuIGV4s6PyJg" +
       "MmZDT3rgtg9OHqmIORFSZ0UHPi+2RnGir86JnXr/KuRJxvZv+9/Ku6RTNws7" +
       "V3tX\nx85MNv3fYyfp4tX3vBWwvJYzTV+h0lGqOlr4uW3kiZ6txsMl1alpya" +
       "vm+tUoKcjavXT9csk2c+XT\nJz/6y5QvZXyNCly1WoLXHr9kHx1++s1Hl7z3" +
       "eY4jjoFgSMEFTY0t5XyqFnAZMA+qhrIcCkJdHBur\nsZrtLBN9vGszr5cIRI" +
       "/i8zKAdZNvDcZH3cCXkXz+4MJFrjZNiZzz9PTLD8Q+OVjD9Snnjg47FWbZ\n" +
       "qQwp7N9Cpcq8SpjiNUOZjYWRuk1aT1YeblOHcIc1nO7MyZQjrMj7PgGblRWm" +
       "ISc4OphUzuI+LDEe\nkdDpCqxu4ryv5g2tvP4Mr9v9YICNNYy0cAixucTDuA" +
       "AOVA1RSBwgijfQsW5JEdsj2TNvdQHzBiat\n86NON+4DLdz5wc6aZ/fpb7Tw" +
       "QJrBd4i2EzEyz5VOdbtfidXlyjz3eSHcGWm2MAmpKWyZYkkEMxmj\nPebkwW" +
       "bBgsHZuUHqjm8N/emvzxz9wF4w5jlaeUTrl58vHSq7+pEr/ljEXcWVc811qY" +
       "PJqWurZu0R\nG8IUFlvEXGjuv1VyJYv16lOfPPL3T7waJaV9ZCYPdNitb5XU" +
       "LK5hfbBFNTusRrC55713uyn2T04y\nWOdPBl1s/WmgO+anMU+0z/B4zTx79+" +
       "OaN0J0fNhTGOtFDNgoGYnj0TKstsMsR5Jm6D6z21DSsHEb\ntfaZ9+39tw1F" +
       "uUNtUdfWfGFhkuuisWzPg1pHh17gQ/PC3qcuW/DYns1vDNgr2g0gtWqjaJUb" +
       "iSAG\nL/HGoBVf+ewQ61UFMIa/h3lrpxP363jDdbzeljdyFZTLoKzGEgSpXw" +
       "yDVGHeQMSMim4+FPgElA0B\nrIJyjzZjyMIA+qT5cbn5vQc5tlZkdZ0a7Vo2" +
       "k7LANTqq4NNuX8wjr00BvBjpxPlJa4Y+nM8lrQRG\nbE1ikjGUTUM2HFMysS" +
       "UDyIqmYtKANkpjA2Ox25apA+rupZYtDdLgl3zNGPi2IlvS3/x3r999w7FH\n" +
       "jwmHwt09Bop/H9gmy9Q0u+GtH7s6Q2zGyBUpwcjSwaC6QU0QG4QFMbk+GAkx" +
       "iY8dQ9YxEB1EbvGC\nmVtgN6ZF5h28esO1Zd/LJ155HHTPU17ypCZL6nfv/z" +
       "C958XicnEkkIE8x9iUVSGjiKpKwLaS07gw\nwpUrBVjW4bVZ01hxcsNzkace" +
       "nSOysEIHynf+3LGbNjRWPh6Pes53kMvHrIQKDb0OyvpCQxPvEgXA\n7JG5X6" +
       "7Z3X7ld87QJ8T+U3UnQyGQ4xtgYfOND7+X/sly28y35UW6Hsr2gLm38O+xws" +
       "DkYbgXnKMI\nZ9obfThaX1D04Z+H8t16Q7qdv8Mh/Td4/Uiey1YoNwZYm+sl" +
       "xMfHuzjBYay+BxKsnXToAv1EsYvV\nMRc/3noUq6/kIsLCP8bqcUaiSkYYE6" +
       "sv81n6p4Lsuyc7YDLXkWfN6h2f2qJcf0PQoQxPOCxSP92h\npb8dvWzl3odE" +
       "bjwgmTwgSsG5TOzJSCz8NJ2PJSCkIm9rLAuglGOBVK0ro2ch2zcoiMCTykZP" +
       "Uqlo\nCV8PPt4pe0ESBx2KFnf14qdwBcYA9BuuPXLPfR2v8+Co4vJzmOkRmr" +
       "R4Ux6LaFGPtx8mNrHxzdcv\nH2l898jhQ8dH+CGubSn3MdBGSS84BsJ9MbQX" +
       "l/77Myfn3HIaUrK1pELVpNRaiR+wgbmGwceHNTWV\n063suHJnmWVTsQTn8n" +
       "ZugrLUtjM2PofV83a4nh4nXckfOjnbFJ62YN99Orf/AaxecCcHTsx+Bsqa\n" +
       "ix6zqH/H+DGbw7V8UZAn+Cd1bflHrb+cfmYj94wSNqyYi1byU48Ah4B5Qx6v" +
       "YPWg44s8Gp/kxjnO\nwWL8rR0jlUOUdXMzQxblQ0dMgxomRse5Id2m1tLzod" +
       "SPb2k7Gmu96WFevQgZN0F09oT3ncue0DEE\npowfv+iGWAFl5SQM4U1nkWoA" +
       "i0surDGdtewyFensKigbA1iJNcvTrTug2xRnot7pWRci29ROTxeU\nZCEXYe" +
       "Kj+W43QLklQBgBjJFqa5UNyFmQ8rNhJna03RrSbWq13QblpgBtuQ5BOUtk9k" +
       "XIWSK1Ln75\nnCVSbuUskaYdhTlLpBT9PyIOnZ7B6lne0MzXFN76Eq9/XrjI" +
       "7cgvXY69sdPai27vdiK2P+PYOyc8\n4S1sasGqjKvlKD6H69kwqeVhBi4P4q" +
       "w6eH1onNgT54V0m1rLXGqtQuNYxl4f5vrWh7x+k10gIpHz\nWyAQhC+/6JZI" +
       "ELHVn8gSzgKBh0qLofwjlqAFIjllCwSeW8UDWPkWiCWWDv5ujHRMGjtG6FgQ" +
       "diQ9\ny8MOz+odJNmkT0Sx+yhWexwI2mJD0GdtQB+xwwhbe3go9tqOOavgXI" +
       "u/xqo/YKH99MR2xG7XBCrV\nNmk7mudixatD5LogK6ZsK45YVoyYjg0HuA1l" +
       "24YzuQ3xMDYu7v7wt1hlJgK7UprhgvhMXW3hV6Cp\nHc3rQrqdr+boD9bbDk" +
       "Ufpgbe3prgjNNBqI3nglCdDkXwgagNEN/GEgQQd045QPhZhQCEv9vfACAK\n" +
       "WV6Aa3/edu0HAgHibu7cn+MI4DwFYMG1E5tslZWhFMp/Lkm3nmXnYrRPh8h2" +
       "QUZ70Dba1y2jFXO5\nHLPx86jIYTP0Gh6/jiLOlur3nvjlcMWBY1H+DcY+Wy" +
       "r3X2MsvKXouXzIQ2G6x9atYVPCRfyWHz+u\nCiE5X0vt0wWH/Qy0GmNU13Vh" +
       "FKy/6RLBEgfr72D1KPS8QKgMVLsQKqdAVRR4bNKouO+cUXHfBKi4\nEMpJLE" +
       "Go+MyUoeIiKMsCWPlCfJEFUf5uFxEVF4dIdkEB/pId4KcDUfEF7qwv5lHxxS" +
       "BUXGmhzwQm\nw27tgfKvmbTJZFifzydJmlKbvWbb7A3LZtNQLMdov+JGe5UH" +
       "jPMU7IRl3AnLfBatgBKDcgpLeHTX\nh3S7gERogTcx7sqMSqqSgvw4f9/CJy" +
       "reIGmBUkT4iXJ9l6rSIUltVzV5pEfZRce7QpJLCFkS4xK5\nUOWn1iezQkSZ" +
       "aSEKmjfyX947JLm4pXIomwCdYo5Ote1SqltKpSDF9WmzPFCbkO7cK35vSxbz" +
       "SBZA\nEiDTQkem+eI6TLBYiUCxwim8ki30SBZMFSBcwhGuxXKb/GWdbtsPfY" +
       "KuDr5QNDlyr9QrAx13nCHy\n8UqoGAkf/UeAqJjr69Vc/JzcrfEExafKMo8q" +
       "/IpKWGev4A3ONiqIIECgZkegOsul2yyA9AkVLxRq\nPAKvYE2OYGFEmJ+kaF" +
       "h+cmnYmuAg2NyQbueZn2Qmvpli6dcp4GSHk3Lgd2sYET/SNYb9+4/4lxV5\n" +
       "uXn/t3c88s06kcja/09SliRlgzCD7stFrucS3aCDCudYJq4a8U9r0QWMlAiF" +
       "8FeTbpvftb0Wd51c\nN09d33512HnD8iOpeLomnp5Y8aUf14xueiX0cnat5x" +
       "vZxqz4N6h++X19+7r/2PyfR8VH44WhXz0d\nivnmjhnbFq4f5J+Li2F2du1C" +
       "fqWQs4ul28rZ/dsD92j2WMqbrw7efedvquyrCO5L+I7qDeHj4PPW\nGa2/3n" +
       "D8+HcLr906Q7jU91xkW7Xkg9IPf/Z/13rtFnHuNuzJ/T8g3BglsDYAAA==");
    
    public RSA(final jif.lang.Label jif$L) {
        super();
        this.jif$crypto_RSA_L = jif$L;
    }
    
    public void jif$invokeDefConstructor() { this.crypto$RSA$(); }
    
    public static boolean jif$Instanceof(final jif.lang.Label jif$L,
                                         final Object o) {
        if (o instanceof RSA) {
            RSA c = (RSA) o;
            return jif.lang.LabelUtil.equivalentTo(c.jif$crypto_RSA_L, jif$L);
        }
        return false;
    }
    
    public static RSA jif$cast$crypto_RSA(final jif.lang.Label jif$L,
                                          final Object o) {
        if (jif$Instanceof(jif$L, o)) return (RSA) o;
        throw new ClassCastException();
    }
    
    final private jif.lang.Label jif$crypto_RSA_L;
    
    private void jif$init() {  }
    
    final public static String jlc$CompilerVersion$jl = "2.0.0";
    final public static long jlc$SourceLastModified$jl = 1396370676000L;
    final public static String jlc$ClassType$jl =
      ("H4sIAAAAAAAAAL16a6zs2nnQnHMf597JTe4jr6so92bn3pvkXDk5tsf2eJyb" +
       "UDwve2Zsjz22xzMO\n0Ynf4/H7bU9IgVRKWrWllDSlLVCBQP0B4QcUEKAiRU" +
       "oFP0ACgQAhEQhVVahoQIiHxB+CZ/be5+y9\nzzlJ20RsaXmvWetb31rf+7O/" +
       "9Y3vdp7J0s4n4shvHD/K71l1fm/v38ub2Mru8VqaWebI17JMagfu\nGz/7X7" +
       "96B7wt33qq86LaedENxVzLXWMUhXm7Tu28EFiBbqUZaZqWqXZeDi3LFK3U1X" +
       "z30AJGodp5\nJXOdUMuL1MpWVhb55RHwlayIrfS05+Ug03nBiMIsTwsjj9Is" +
       "77zE7LVSA4vc9UHGzfJ3mM6ztmv5\nZpZ0frxzi+k8Y/ua0wJ+gLkkBjxhBK" +
       "fH8Ra867bHTG3NsC6XPO25oZl3PnJzxQOK31q0AO3SO4GV\n76IHWz0dau1A" +
       "55XzI/la6IBinrqh04I+ExXtLnnnQ09E2gI9F2uGpznW/bzz6k04/nyqhXr+" +
       "xJbj\nkrzz/ptgJ0x12vno40V3RWg/Fzz1577yzd/94O3T0U3L8I9kPNOu/d" +
       "jj164s20qt0LDO1//Yl+48\n/59/ixre7nTaNR9+/Jpz0O/c/+b7//nh/3z5" +
       "HPSNJ4Mu9b1l5PeNb/+DN77bJT/5paeOZ3sujjL3\nqCbXuHKSOH8x804dt+" +
       "r6gQeIj5P3Lie/ufpH2z/516z/crvz7KzzrBH5RRDOOs9boTm66N9p+4wb\n" +
       "WrPO0377r2WH7frWkR1Pt/1Yy3enfh13Op07bXupbXePLe/cWYnkvb1r5533" +
       "ZFGRGhZ4MVAfl7y7\nunWrPdibT7Ajv1U6OvJNK71vQB9x/v4/nP67j91+oE" +
       "kXe+adrpE2cR7dazF3bt06YfzgdVKPvDOP\n6v97f+udl/70p7K/e7vzlNp5" +
       "3g2CItd0v6XoBc33o8oy7+cnvXn5io6edKLVqxf0VsVabb3vt4hO\nKt0SXF" +
       "6V14UOPTTAWdvTWo34Yp/rfP31ya8cBXyUxPuO2M+P1vLVOz/bC2+Ln59/4S" +
       "ffeOoIVD3d\n8u9ICfAERX10k/vG7/3GO7/DQL8lnevR20/gahoZltl6kofr" +
       "foL6b/d/+2uvq7c7z7Tm0zqQXGvF\n3Frj6zfN55qOv3NhF3nnjUes8eYm71" +
       "x6miPjbjOdd9lRGmj+Ec2le+jmuzSqHo6cNORdp/57vnf+\n938v2vdaLzKK" +
       "grjVwvSMstqzarllxuc6dXycHZl8Qy4nh/Y/Zl+lf+ef3P387au+78UrTlK0" +
       "8nNr\nefmhjKTUstrxb/8S/7Wvf/ernzsJ6EJCeefZuNB916hPB33/rVYh3n" +
       "tj4yNJ91593y/84tt/4d9c\nasB7H2In01RrjgpQ/6l/8dov/2PtL7ZW3Vpa" +
       "5h6sk0XdutCFI/5XWjtqjefeUTHvMZpu+Zf7Hp/A\nqf/JFrpzWtOpL2aPav" +
       "rhm+w4RoFL8QT6F//Xt361e3Z+vOOaD50wPJ090d1dW3/f+J9/+/6tP4a9\n" +
       "9hMnxj7UoSOq1+snmvhau6K+r/l/B/vVP/E3//Xtzh2189Ipjmlhvtb84sh9" +
       "tY1E2ehikOm8+9r8\n9ahy7iYfqueHb6rnlW1vKuZD19L2j9DH/p1zXTw+Pl" +
       "rfaoX+TO8edA8+/v70aeGbp+fHz5XiqXbe\ndkPtFDBaH/hsdor3dd754N43" +
       "3rpU3HUb8lv3+1Yrz0vZvnTSipNwzyPjSaFPs/3LzVuJvOchGBO1\n4fOnf/" +
       "vP/NOfe/M/tKyfd54pj/xoOX4FF1cc84uvfOPrr73rF77z0ycZtwo1ef1Tv3" +
       "x2xDo6Pj7T\nxt7j6cSTl2a0LGcj021TBfN0wCeqAZ+6QRtGyouo9/Nf/reL" +
       "p+pfJG9fSRTefNQ7XFlzniycGNyN\n6+xqaH7MDifo3wQ++o0fX/17/dzPvX" +
       "Ld10/CIvhPzbesT3zmZ//jY4LF0370WLbmH8BoNJuRl38M\nrFsIadRqWG7W" +
       "Pt9gk3w0yHpWUvsigjn4IR2S24XhjCRmVeraaFL2qNUkW6VsrpnjrHsI3WAN" +
       "99XM\nAdgxmM9G5NStG2k4rZlmMVlGZG3Pa1kWJ1iO7NRGGhVSwZKmvI9YRi" +
       "qiIpIXiVcsgBTJgy6GTw5e\ncGAAjdOzA0IQth3hYAnDh15eD/riNmcdQjcS" +
       "B5n0Yp89lD6q4vMt6/W1PrdRsimW9rE1aJiWtOuq\nfaJPgF60kBPRcENmt1" +
       "+Pt8qSPUwO1iKCF65YS/BkJbJrP/WyWmZdTWK9ZkVNejmnC9CsKsSlU8Ex\n" +
       "FWVdYcNmkVxQ/gSr1Cgsxr7cSCKZohoSjNMFvaJnlKLUUj2buHaSJ424naCH" +
       "1SgLcyDHB9DEhMSF\nOF/b2mrRbWyhckW/2WHqTuBMepGzEC+TXNb01lHiAa" +
       "ybjzUtoan2xyItnAjT4xmhBGwsrhROc7BA\nTbQiisyk6nWzQwQvJwDOyl6i" +
       "CrDM4rMGqqrdYuYQyZSEVXuQ0L7oz9TlarnFR0LWg3skpq1dxwnG\nk6gxhS" +
       "CQNFedLtgux0fxYB8VPYc0HUaAJIPOrLUusbsZ5FFRTWqJTMkpxE74+XhCb7" +
       "YuxGyGrkof\n2tOMUmm+8SdUwhB79gB3CSBT0v5ws5SMibcfNq4+0OnKCzSI" +
       "rlQmCPomBkGqXS7LqaWrIhMmi8YN\nEpkM4AO7hIOl53BBTMURQwRdsxdiRQ" +
       "8oNXQgkvsVXEmi28dKYt7gGAzDIOIWfS5aI+UGd9ZucrDL\nkB1o2mGqIIxZ" +
       "WzudUvqqm0bUsJ92Db4NgnkfwkJhFoxDzLGnPkJj2LbkDhyBAT6P9kQv6Yuz" +
       "IEtM\nH4qjQoagJqmQqNhZluclU1OdRwt7QBpdB40X0KryB4ua0ecOiCGrkT" +
       "gK5GA9UrYU5Zk0xyt1NfJV\nig/WqeJR4UjLnJ1obpYiKUyU2hGXYoQiG0Xr" +
       "BsmcKvGmMSyX5uEUt0IWwiFPkaBtolIcq8KRsXK1\n1WgI1HCSJT23P61sHP" +
       "D60KHExyNw3HeE3SBeE0zcZeYsteZGkezP17pocoTEFxzOGXyQ0BYgk8Vw\n" +
       "h2r4YEGigQMPgsjC0YxG5qM0KveuV5dFbyYtzb0ArBSju503c2pXmy6p9ugV" +
       "1axFPC82IcEIluam\nY+kQ+A5PiF6z5RpS4ZGGIECA8ntYIB3o/sIrZqC8WG" +
       "61pbvzu2tdU0lv7nn5jFzb5Qynq0M1Q4YW\nVYjS0K5mE2rWgyZ9ZDOeUQuf" +
       "I8MsNaerTCzJzYjMA3vXeDop0aVYed04mnt2CcZwaIkLAiymYh/k\nWGE4nv" +
       "SCxZbEdgC+NHWwBAfQeLzpOQtI6LssQVqkuByGgGe1koOFeLeKo0XXYXk+5m" +
       "RqZa5Syt95\n8pgXvSgd9yZasmWXVJ5zMWqgA7Kfq1UC1EokSLjNcuMkBvtc" +
       "DCJSVjK4h/Uz3e4ucquBJ0vV5Xbq\nPgYhKl6NACwcI8LA4JkSnBeo58dGpc" +
       "yChuxHLoY7UL6aVk3uAbW7S20K5/wKzUEzTtu00V6anlqy\nVR2P51BlDoyy" +
       "vyMGmzCaeaTiIutVT17h6x3NupyzzNbgOCyWmb+a42MEoXUuX05GwsJR1jDe" +
       "RZc9\nG8ZBcQHu8dakHbGGXDw41IQauTwztcskTXFzic0hC2gdjDJT8nhj5E" +
       "LTg/Vqtk5AXVAQiQDRASR1\nCU5qqD7rjfylGiVqthjHi+1guhmZvOLUfrW0" +
       "loXuOaqMygNhvOszCKgG5pJbl4bBEoyW7qkgVnht\nO6u4LlTvwpGkjH21Kj" +
       "AIjg+KULMiqGiDYSxXFCQY0h7KkwE9hky7RJoGB/oEphzoug/K6GoWYZYb\n" +
       "8QbOHCC4m4WILg1wzSPKTBrhZEGmzLDYZUyeiKKSxep6OIRJDilSBABzHrHL" +
       "TY/ZMvQg8uOQVzHc\nkno+boPJdsp3xYlEjVmm3i4s3ViuNB4YmvNWmhUPbv" +
       "pBTgwHg0yrx4ONhRWlBADYbrCky2RD9ysx\nmLrzfchw+Vbajve83eWBgAgC" +
       "nAPgHNnwGKr3tfWyzpe9eNM70NAC8CpU325HjDdhsD6iLqvMlF1t\nn20XGD" +
       "AHUxsxXR/C7IwOym5ezKg1mvX5Zu6wgTqWkNTJHWJJJ8FsuZqKKiTTvAv0Vi" +
       "HlpoP+PNyk\n4KJYzetSLQGi7fZXEldl+BrJN916gLFcidjbxo7RfIOUQ6Uq" +
       "59uDtAZ2kE8lSz9FCwXxXLWf1xnr\nw5ofN1oZALkcLYw2G9tIhJF5PQPYSN" +
       "35Aew3mFCqm4Yb+Q2sRMyGTgmPrmEAyGDaxw+ItUX3+2jq\niOxkKm2hHNs0" +
       "yTQkVZYEkl06M0be3KSHRdrN1DRnhhxRUtkhxYzZaga6g8OCNFWuHBsqjRij" +
       "jbc5\nACg7n6Z7lXPVw4HgJWY8EIDlej8mxDDcOyg+VXS1mwhVjHPEKDaSgR" +
       "jvXU5eqvutsio8WJPgraX7\nS9qYGXC4xNlGFfYpu8d5h9L9lRLMRANdxjv5" +
       "UMwAZjyxup45jkdJXaBpU2JOLy95lE2JktjORhy3\nbSCDdOvCkMYFSS7XJL" +
       "wSh5Xj7m0hU7D5xMz2PYkrTYE+DF0t7w763JID0DFjHrDK6GsqaMc6LlXC\n" +
       "PB2aqNubHCbNAVu3Pi23XXPjN/uWhyJtUErQY8kgXiiNuvW8CVsH+y5v6V6l" +
       "6EMno9VCppN9MOBA\nKygP9YGopmiF7X3NkqXUxwYlLSY4EE9S0N9PNoayNg" +
       "YwnGz5eaIUqVpAqy41mMlTv5xsqH1Rwutm\nvi3hvr+0eZ5BJLzS5tqaUEdb" +
       "3uTcXMc2MrPAdrY+nHJS0WaMIwzDYaRowp4EoU1XWZr80IczUAmc\nJaCFrA" +
       "xAB3CIHqQaJ4ClxkFzyOZ2ErXMp2POiahtuIekRZZnbLbC5kF/3D/04LGY0R" +
       "O9m9uaSgAI\nkVfwtJC5XDZtxaamO68o3Ew58A4c0zsYdwSQHyGJUSjgeBtm" +
       "cNkiTnsO5ORKstfmsu6KJNVlwDb1\nGq5twKqEPixP2ZkG7hA/jWse6SP7sa" +
       "kfLH9vNlNVYqLFgZo4YrypmvGaoi2BFVk3QIEp70x8Ehp2\n09m8FZC6b9hZ" +
       "q88SZughVgoGlvuqZ6qQhq+jnoczJCclCcpxxWZQ7VMIGpbDfbufP9fQLdCm" +
       "Mn4q\nrKiuPoFdb0/Jksz26CnbZKI8bmZJsM0XJOfFe7K3rHuYFpNAWsTqNh" +
       "+bjOMjB47i3d7ML0UErJGU\nIIgKn9LdfRXQVQ4RwygGacIJGkKACoDKaTzr" +
       "C70D1QzluDL2YqWN+oIMCRQqTFndAyjSq93twBSI\nbT0ZwhO9pEbdYJ07XF" +
       "+dYrvewMqWsGqCzBzGMJqWeBnGm3ikMv2wDdOHEYENNq4RTjxvNhnUhCuv\n" +
       "RWrvLuJFxWgihDtVt4rldLjeK6i+ViF4VxsZd/A0wwrFZZNNZnQT2gDCUDq3" +
       "Xyn8rs9bGYZ4bEmn\nM2Ex1qFlU8YjoebLibMhu/icwjZ9XEU2uG8OcGE+kA" +
       "AZkUOhoQS0zp2tveTGOLABM7/pDRoEk5U+\nnbVawQTIYAnUVTb2trqzCRfs" +
       "rKsSkW5LO20DjyFUBehBDvcFPDHLAbsDJRMFZbcN+duIq9RtAGR+\nzdp0cd" +
       "gPt1oDDOtFFm6kiCzhSbYt1112ZYBIGMlJH/RmGTPiB3piyJoretvtpJ+VAz" +
       "jsxyiTFY2B\nb8fMxlNsrr/r2b1kbtgQL0QtjctRLfJEb9eN9YFZKgcficyJ" +
       "vq+cQKocMaMwaXmYi6PDgJv3+Lbj\nzQIiZcjC8anlcL2A0qhn7HoHI3LZkd" +
       "R4Ki8nftQdrQc8n6OxBU3EYd1bTEMbRhOzGhpbkhgWBb9h\neHvVKwfAKELL" +
       "+aHN1VdBukLlus9oU4pv39YCLqXrg2mUXbrnD4jBficMTBoYUYUuhghrDilR" +
       "HKDD\nbE6wurDxQbx1s3aI0DNNThdwPGHWMbfS3BVQOXpAVYZFc5Ky7kJ9JC" +
       "voeRgoaykTpHAQqoWTMnK+\nZ3t8ZK8HiIaGOzTcGJ693If6dN7vbTmCWKY2" +
       "SGq1ZYwMWspqYJglWtet+4eEBmlgTOkVvh7IKhkC\nIxPWbZ9OdcCwg+2eQC" +
       "uRS5qtC+uN3OZ9O3SICXW/3La5DYpmAyoGvXofLruVwgzSFbT35aVsbtiD\n" +
       "UOu+AJmsU0fBKrOwmdkL4Jw5RKbI663JU0FB9JWpVQqOykFTiIgkJDQKCxpP" +
       "Jm1OW9kl4FIKOfUx\nUWBRGQ49JSJdKCvSMWXmVtVMrTBbuI5Owu6cM0oplQ" +
       "qnV6/CKMdjZWW4PWK1lGpL7qqjyWZrLfaA\ngiIC4/nC0Bnu59NEw2SumQbC" +
       "IOV281CDkFUq6koCI4Lux84wLwRsuJRzj/A2M0VcwEyIY91wtBb1\nQs48bW" +
       "vuJJ+JhxLBN7NlqOLDvh9aEHvYTDm8P0kIcJPuZ7jhlJttn8hMXnN7eGm6ix" +
       "4P8NtlYwy6\nqAhnWkYl+0XpL8Z9uk94VWKtd0MX5dGk6ls0HIWcI2HoGgrg" +
       "lNlQSS4U3Lb2URsrU2WH14AHQ84s\npg9dpSGF3QgA+3N1NmHodThwxIOzGO" +
       "6ghInsau8n5HS29/1RJVrKQB4rvKe4I0UZl1ixp1cstc+p\n/tQRTFcgus2A" +
       "XR6AyJxzgSdBYg00O8Wqq1kvmI+hlv9C+wLWvmXrFuJERJ4sl1rpbGYFWCcR" +
       "uF3Q\nBMGuejWFb4ajdN1tceBBYbq1lo8xJnIW/TZJRrHtOmq2eweiCxL2e+" +
       "j2UAhIzY63BV7iVQ7v/Lmz\nwOU43Tnljt0oyG7dH8Vd2kBSIU3MFNCjMaHr" +
       "Y0+2Y6c9SJw6e44bJjMkdPyROfY2laM53IQD2wgl\nCCaEYgK/U3fCaFISpU" +
       "2OyEVXWfdJDQEts4Bo0qFdvcCNEGBn6LiablkKnmB55qvKbsrGIOMFSBhs\n" +
       "wm264neDXU+QVRwa6xrc9PbTzRLr0gxfD/r72VbiIWrXB/cus/Tb1Dos977B" +
       "KsMt3FqasQWUJkU2\nPTYrF5uo2BxmQo/q14SWb9f8aGU3Cn0Aiu7InWAbqM" +
       "yhSqFBoYGogJNhZL2bLhqwv2KGE8dO8d4Y\nXvDioH2rYddMJiwSk1yvVqjO" +
       "LaJiCRjyhraHCxftTth4zkyoCGAbObJm7oLNRgrvj+z5ZCrbWDGt\nJ+spas" +
       "5MbzIE45mJLoyxbE+meFKVmE/XtTiUN1SwO1RC0K16eO7YizLQpbk6de1DY8" +
       "4Df6GXiq7j\nJMKoOScwrBmVwJyyMJne86yMz1Y+QoaMPdRyZ7gYG07MWLS0" +
       "6e4QEXcmq8iRjBrPElZMEtEpK9Qm\nwNG8Sa0ZFQxlT0LdzZzw+6k4k/18fV" +
       "hj8cRWV4aFCy1ho6knovUC68pLJYRd6yB4oqqJk0Wwt5sN\ngeJzJkDjzSwK" +
       "qpG5jbBlbsFi4ueLWBxPCSU48NOR7i1xY4FgpVjrB3Gokt2JO5YEzwoAa9aX" +
       "mOHW\nlUd15QYDz0WgFoCdikOj2lFFvVnoIKW02WjG29tFMU4xgRv7TUoIKV" +
       "nDSDiim+5ATqI2b7BB2SKT\n+T7tE2sDnB+M1J5Mas1esNEgAWdt6kpZJUoi" +
       "S85NZrPJyFE9q32VLfxe05AE6a2ZepV0eXJRbie9\n5ZaaToI54GnTHWAE4T" +
       "YoSKfHNYJeWGrYE4Iy9t2+acQoJdIewjhmL9tOJFDktjoBoCDq12usW2xC\n" +
       "MEeM9t0N6nvt6wsIEaLvrnznMBrjWT5NC0NEtNF4VJjbeYJqA5lIIUFhfdKd" +
       "DqaWpMelneuQYmKw\n0eUgwkQQDO7R4XjgLmOLiYIwxrYDTW4DaF1Y7gYyQi" +
       "3H2jevOjmgbTgHgA1MAHgK9vjGXg3WNIoq\nu80BMrtuGQb7XRvCktbZzQHG" +
       "7U17ZgGP7EQRMrTlySBVQTMbE719T2ETPa1wy7bMMimadLTEGGCG\nJ2OL3q" +
       "M8r3QprdfmGuMBQBtMSkBt0rKXCbj2FHzjOcSMt0EVhJE0n4Z0wRWHOSgNHG" +
       "To+Nl4S5Lk\nZz97/H68vPiA/vLp8/6DKvjetY8T5KNfmzvxsbN6tHRw+9j/" +
       "I3nnTpy6pXa8HtB5qUXz1nmF8/5K\nJO8zR5B7J5ynj+YfuyjxPCwAfeii+J" +
       "Olnddu1iavlei/uvnvL3xF+83PH7+jHxcyeef5PIo/5Vul\n5T+sHN1Ewp6q" +
       "+pcFlL/87Cvm08zg1Zulo3e123/88TWD6wjuG9/7pd/4zl+/+7s/c/tmAaab" +
       "WnmR\nhtK1Msxr5yXBdotX2nasdt07tuPgS8fHyw/LgNoTOHz3+Pixx5fOLs" +
       "ptJy5/X4jLos3HT4WWzDKK\n1M2be1wkFsaO9J2o/bkLJrVhxccyeyvKF52L" +
       "UuXCatqgn15ieP91DFdnv/CA2BfbBrTtM8f2OGLD\nPwSxT50XrE7EXp7mvD" +
       "TpRvdmYVzkYp5aWvB9uHFZasw7LzhWzp+KoS0Fl9g+eJ226/PXqftU2/Rj\n" +
       "exx1X/whqTs+8t8XGe8+knFuflfoePUGHdcBvnBNJz/Rtp86tscR8pUfkZhe" +
       "fkRpTs7mB1F3xwpP\nzuQBmovLEyM33lnp0eM8gaBfO7bHEfS1H4FkfiZ74p" +
       "WcU3n8vLj42pe/+S933Z/89dun4qyuZede\n4eaVpkdvLF27iHQ6+fMP6EPb" +
       "9kcfQ98JgI7P/y/ydr8mtx7rA05e/c9fevW/9CSvfuJJK4FnfSt0\n8t1N/E" +
       "+1pz52f6V+sMnt89XXDfNU0R35UWgdb65czr3v0mgfXBxrJ+vHHvcL58c97X" +
       "V8/I3j42Pf\nV21+gE49YxyP85gi9nkhvv796uRx5ucfo35vtu1bx/Y49ft7" +
       "Pwr1O+38eNjnTrDPXVL30etmNwvL\nlttma30PHP21uFDfu7Cvme9bjuYP/c" +
       "jwRPdgPQJ+dg18qLWvhabphs4jgG9eAzwPN0+ChR572gfB\nidfS45U8K32w" +
       "8LjOvVz9+tUrBL7PRyfDemSTNx6CXVBJpk4RWGF+NfjdMa0HQiYfKuKLDyX5" +
       "z35o\nRXzXOVPeavOkt05obyjT8XLax4+GcH5154+f0qGxZfhSNG9pmNT5fe" +
       "PekZqzu8bl9aLLmG2+/WkE\nRT59lhRa5iZFlFt3z2/+nJWRa54dszQ3LCPP" +
       "Glv2ldtZd98++2K+c7N7V4529+13vvT2ldtK/+qH\nJvzVJ23/iBc7HvYGV4" +
       "7ttQdcufXBPyBXesh1rpzfdjm7YI4eRb6lhSf+XGZ8kX33c6cLMmfX7zJ9\n" +
       "UQv0L50gmU+enQOce5Dziejz05aZrn12NzpzH6A6aznaDh//nRlnnz27e/wd" +
       "vXN2njre2EE+3hCx\nkqIN3q0XzqXornHvZn79ydMBWhFdYLA1P7OuC+w7fw" +
       "ifc/sh2Mnn/PoPkul7rrPspiTvXHD2qjCP\nV+Paxa8eBXnsHwffuHLu7/7/" +
       "OPd7TwzVsvwKVx9njM+17eWHxnj/D6h2MAJdN8bzjOyqNbr50frO\nPvd58e" +
       "ya+P73o2w4USN/f9J+AN3PXW56nPp2O/BUS/jxrt9HnnRN/Pwys/HJ7M/+Wv" +
       "JX/8qHz9Oa\ny5vGzzKd5+zW6169j3al/2ycWrZ7ounZ89tpJwJv3W4TjHO+" +
       "H389FZ+L7P8Bxx42s7EuAAA=");
}
