unit KRBCrypt;

interface

uses
  {$IF CompilerVersion >= 23}
    System.Classes, System.SysUtils,
  {$ELSE}
    Classes, SysUtils,
  {$IFEND}

  JwaBCrypt, KRWindows, KRCryptCommon;


  procedure KRBCrypt_Hash(AStream: TStream; Algoritm: String; var Hash: TBytes);
  procedure KRBCrypt_SHA1(AStream: TStream; var Hash: TKRSHA1Hash);
  //procedure KRBCrypt_MD5(AStream: TStream; var Hash: TKRMD5Hash);



  procedure KRBCrypt_SymmetricEncrypt(ASource, ADest: TStream; Algoritm,
    ChainingMode: String; var Key: TBytes);
  procedure KRBCrypt_SymmetricDecrypt(ASource, ADest: TStream; Algoritm,
    ChainingMode: String; var Key: TBytes);

  procedure KRBCrypt_SymmetricBlockEncrypt(ASource, ADest: TStream; Algoritm,
    ChainingMode: String; var Key: TBytes);
 { procedure KRBCrypt_SymmetricBlockDecrypt(ASource, ADest: TStream; Algoritm,
    ChainingMode: String; var Key: TBytes);}

  procedure KRBCrypt_RC4_Encrypt(ASource, ADest: TStream; var Key: TBytes);
  procedure KRBCrypt_RC4_Decrypt(ASource, ADest: TStream; var Key: TBytes);
{  procedure KRBCrypt_AES_CBC_Encrypt(ASource, ADest: TStream; var Key: TBytes);
  procedure KRBCrypt_AES_CBC_Decrypt(ASource, ADest: TStream; var Key: TBytes);
  procedure KRBCrypt_AES_ECB_Encrypt(ASource, ADest: TStream; var Key: TBytes);
  procedure KRBCrypt_AES_ECB_Decrypt(ASource, ADest: TStream; var Key: TBytes);
  procedure KRBCrypt_AES_CFB_Encrypt(ASource, ADest: TStream; var Key: TBytes);
  procedure KRBCrypt_AES_CFB_Decrypt(ASource, ADest: TStream; var Key: TBytes);

 }


implementation

const
  KRBCRYPT_HASH_BUFFER_SIZE = 1024*1024;

type
  TKRBCryptBuffer = array[0..KRBCRYPT_HASH_BUFFER_SIZE-1] of byte;
  PKRBCryptBuffer = ^TKRBCryptBuffer;

procedure KRBCrypt_SymmetricBlockEncrypt(ASource, ADest: TStream; Algoritm,
  ChainingMode: String; var Key: TBytes);
var
  AlgoritHandle: BCRYPT_ALG_HANDLE;
  KeyObject, VIBlock: TBytes;
  ObjectLength, VILength, bytesReceived, dataSize: cardinal;
  KeyHandle: BCRYPT_KEY_HANDLE;
  Data: PKRBCryptBuffer;
  Data2: TBytes;
begin
	KRRaiseNTWarning(BCryptOpenAlgorithmProvider(
    AlgoritHandle,
    PChar(Algoritm),
    nil,
    0));

  try
    KRRaiseNTWarning(BCryptGetProperty(
      AlgoritHandle,
      BCRYPT_OBJECT_LENGTH,
      @ObjectLength,
      sizeof(ObjectLength),
      bytesReceived,
      0
    ));
    SetLength(KeyObject, ObjectLength);

    KRRaiseNTWarning(BCryptSetProperty(
      AlgoritHandle,
      BCRYPT_CHAINING_MODE,
      @ChainingMode[1],
      Length(ChainingMode),
      0
    ));

    KRRaiseNTWarning(BCryptGetProperty(
      AlgoritHandle,
      BCRYPT_BLOCK_LENGTH,
      @VILength,
      sizeof(VILength),
      bytesReceived,
    0));
    SetLength(VIBlock, VILength);

    KRRaiseNTWarning(BCryptGenerateSymmetricKey(
      AlgoritHandle,
      KeyHandle,
      @KeyObject[0],
      ObjectLength,
      @Key[0],
      Length(Key),
      0
    ));

    try
      ASource.Position:=0;
      New(Data);
      dataSize:=KRBCRYPT_HASH_BUFFER_SIZE;
      SetLength(data2,dataSize);
      try
        while ASource.Position<ASource.Size do begin
          bytesReceived:=ASource.Read(Data^,KRBCRYPT_HASH_BUFFER_SIZE);
          KRRaiseNTWarning(BCryptEncrypt(
            KeyHandle,
            @Data[0],
            bytesReceived,
            nil,
            @VIBlock[0],
            VILength,
            nil,
            0,
            bytesReceived,
            BCRYPT_BLOCK_PADDING
          ));
          if bytesReceived>dataSize then begin
            dataSize:=bytesReceived;
            setLength(data2,dataSize);
          end;
          KRRaiseNTWarning(BCryptEncrypt(
            KeyHandle,
            Pointer(@Data[0]),
            bytesReceived,
            nil,
            @VIBlock[0],
            VILength,
            @Data2[0],
            bytesReceived,
            bytesReceived,
            BCRYPT_BLOCK_PADDING
          ));
          ADest.Write((@Data2[0])^,bytesReceived);
        end;
      finally
        Dispose(data);//Dispose(data2);
      end;

    finally
      BCryptDestroyKey(KeyHandle);
    end;
  finally
    BCryptCloseAlgorithmProvider(AlgoritHandle, 0);
  end;
end;

procedure KRBCrypt_RC4_Encrypt(ASource, ADest: TStream; var Key: TBytes);
begin
  KRBCrypt_SymmetricEncrypt(ASource, ADest, BCRYPT_RC4_ALGORITHM,
    BCRYPT_CHAIN_MODE_NA, Key);
end;

procedure KRBCrypt_RC4_Decrypt(ASource, ADest: TStream; var Key: TBytes);
begin
  KRBCrypt_SymmetricEncrypt(ASource, ADest, BCRYPT_RC4_ALGORITHM,
    BCRYPT_CHAIN_MODE_NA, Key);
end;

procedure KRBCrypt_SymmetricEncrypt(ASource, ADest: TStream; Algoritm,
  ChainingMode: String; var Key: TBytes);
var
  AlgoritHandle: BCRYPT_ALG_HANDLE;
  KeyObject: TBytes;
  ObjectLength, bytesReceived, dataSize: cardinal;
  KeyHandle: BCRYPT_KEY_HANDLE;
  Data: PKRBCryptBuffer;
  Data2: TBytes;
begin
	KRRaiseNTWarning(BCryptOpenAlgorithmProvider(
    AlgoritHandle,
    PChar(Algoritm),
    nil,
    0));

  try
    KRRaiseNTWarning(BCryptGetProperty(
      AlgoritHandle,
      BCRYPT_OBJECT_LENGTH,
      @ObjectLength,
      sizeof(ObjectLength),
      bytesReceived,
      0
    ));
    SetLength(KeyObject, ObjectLength);

    KRRaiseNTWarning(BCryptSetProperty(
      AlgoritHandle,
      BCRYPT_CHAINING_MODE,
      @ChainingMode[1],
      Length(ChainingMode),
      0
    ));

    KRRaiseNTWarning(BCryptGenerateSymmetricKey(
      AlgoritHandle,
      KeyHandle,
      @KeyObject[0],
      ObjectLength,
      @Key[0],
      Length(Key),
      0
    ));

    try
      ASource.Position:=0;
      New(Data);
      dataSize:=KRBCRYPT_HASH_BUFFER_SIZE;
      SetLength(data2,dataSize);
      try
        while ASource.Position<ASource.Size do begin
          bytesReceived:=ASource.Read(Data^,KRBCRYPT_HASH_BUFFER_SIZE);
          KRRaiseNTWarning(BCryptEncrypt(
            KeyHandle,
            @Data[0],
            bytesReceived,
            nil,
            nil,
            0,
            nil,
            0,
            bytesReceived,
            BCRYPT_BLOCK_PADDING
          ));
          if bytesReceived>dataSize then begin
            dataSize:=bytesReceived;
            setLength(data2,dataSize);
          end;
          KRRaiseNTWarning(BCryptEncrypt(
            KeyHandle,
            Pointer(@Data[0]),
            bytesReceived,
            nil,
            nil,
            0,
            @Data2[0],
            dataSize,
            bytesReceived,
            BCRYPT_BLOCK_PADDING
          ));
          ADest.Write((@Data2[0])^,bytesReceived);
        end;
      finally
        Dispose(data);//Dispose(data2);
      end;

    finally
      BCryptDestroyKey(KeyHandle);
    end;
  finally
    BCryptCloseAlgorithmProvider(AlgoritHandle, 0);
  end;
end;

procedure KRBCrypt_SymmetricDecrypt(ASource, ADest: TStream; Algoritm,
  ChainingMode: String; var Key: TBytes);
var
  AlgoritHandle: BCRYPT_ALG_HANDLE;
  KeyObject: TBytes;
  ObjectLength, bytesReceived, dataSize: cardinal;
  KeyHandle: BCRYPT_KEY_HANDLE;
  Data: PKRBCryptBuffer;
  Data2: TBytes;
begin
	KRRaiseNTWarning(BCryptOpenAlgorithmProvider(
    AlgoritHandle,
    PChar(Algoritm),
    nil,
    0));

  try
    KRRaiseNTWarning(BCryptGetProperty(
      AlgoritHandle,
      BCRYPT_OBJECT_LENGTH,
      @ObjectLength,
      sizeof(ObjectLength),
      bytesReceived,
      0
    ));
    SetLength(KeyObject, ObjectLength);

    KRRaiseNTWarning(BCryptSetProperty(
      AlgoritHandle,
      BCRYPT_CHAINING_MODE,
      @ChainingMode[1],
      Length(ChainingMode),
      0
    ));

    KRRaiseNTWarning(BCryptGenerateSymmetricKey(
      AlgoritHandle,
      KeyHandle,
      @KeyObject[0],
      ObjectLength,
      @Key[0],
      Length(Key),
      0
    ));

    try
      ASource.Position:=0;
      New(Data);
      dataSize:=KRBCRYPT_HASH_BUFFER_SIZE;
      SetLength(data2,dataSize);
      try
        while ASource.Position<ASource.Size do begin
          bytesReceived:=ASource.Read(Data^,KRBCRYPT_HASH_BUFFER_SIZE);
          KRRaiseNTWarning(BCryptDecrypt(
            KeyHandle,
            @Data[0],
            bytesReceived,
            nil,
            nil,
            0,
            nil,
            0,
            bytesReceived,
            BCRYPT_BLOCK_PADDING
          ));
          if bytesReceived>dataSize then begin
            dataSize:=bytesReceived;
            setLength(data2,dataSize);
          end;
          KRRaiseNTWarning(BCryptDecrypt(
            KeyHandle,
            Pointer(@Data[0]),
            bytesReceived,
            nil,
            nil,
            0,
            @Data2[0],
            dataSize,
            bytesReceived,
            BCRYPT_BLOCK_PADDING
          ));
          ADest.Write((@Data2[0])^,bytesReceived);
        end;
      finally
        Dispose(data);//Dispose(data2);
      end;

    finally
      BCryptDestroyKey(KeyHandle);
    end;
  finally
    BCryptCloseAlgorithmProvider(AlgoritHandle, 0);
  end;
end;

procedure KRBCrypt_SHA1(AStream: TStream; var Hash: TKRSHA1Hash);
var
  _hash: TBytes;
begin
  KRBCrypt_Hash(AStream, BCRYPT_SHA1_ALGORITHM, _hash);
  Move((@_hash[0])^,(@Hash.bytes[0])^,20);
end;

procedure KRBCrypt_Hash(AStream: TStream; Algoritm: String; var Hash: TBytes);
var
  AlgoritHandle: BCRYPT_ALG_HANDLE;
  HashHandle: BCRYPT_HASH_HANDLE;
  HashObject: TBytes;
  ObjectLength, HashLength, bytesReceived: Cardinal;
  Data: PKRBCryptBuffer;
begin
	KRRaiseNTWarning(BCryptOpenAlgorithmProvider(
    AlgoritHandle,
    PChar(Algoritm),
    nil,
    0));

  try
    KRRaiseNTWarning(BCryptGetProperty(
      AlgoritHandle,
      BCRYPT_OBJECT_LENGTH,
      @ObjectLength,
      sizeof(ObjectLength),
      bytesReceived,
      0
    ));
    SetLength(HashObject, ObjectLength);

    KRRaiseNTWarning(BCryptGetProperty(
      AlgoritHandle,
      BCRYPT_HASH_LENGTH,
      @HashLength,
      sizeof(HashLength),
      bytesReceived,
      0
    ));
    SetLength(Hash, HashLength);

    KRRaiseNTWarning(BCryptCreateHash(
      AlgoritHandle,
      HashHandle,
      @HashObject[0],
      ObjectLength,
      nil,
      0,
      0
    ));

    try
      AStream.Position:=0;
      New(Data);
      try
        while AStream.Position<AStream.Size do begin
          bytesReceived:=AStream.Read(Data^,KRBCRYPT_HASH_BUFFER_SIZE);
          KRRaiseNTWarning(BCryptHashData(
            HashHandle,
            Pointer(@Data[0]),
            bytesReceived,
            0
          ));
        end;
      finally
        Dispose(data);
      end;

      KRRaiseNTWarning(BCryptFinishHash(
        HashHandle,
        @Hash[0],
        HashLength,
        0
      ));

    finally
      BCryptDestroyHash(HashHandle);
    end;

  finally
    BCryptCloseAlgorithmProvider(AlgoritHandle, 0);
  end;

end;

end.
