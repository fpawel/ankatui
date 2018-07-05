object DataModule1: TDataModule1
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Height = 618
  Width = 892
  object FDConnectionProductsDB: TFDConnection
    Params.Strings = (
      'Database=$(MYAPPDATA)\'#1040#1085#1072#1083#1080#1090#1087#1088#1080#1073#1086#1088'\ankat\products.db'
      'LockingMode=Normal'
      'Synchronous=Full'
      'JournalMode=WAL'
      'DriverID=SQLite')
    UpdateOptions.AssignedValues = [uvLockWait]
    UpdateOptions.LockWait = True
    Left = 80
    Top = 24
  end
  object FDPhysSQLiteDriverLink1: TFDPhysSQLiteDriverLink
    Left = 624
    Top = 40
  end
  object FDQueryWorksByParentRecordID: TFDQuery
    Connection = FDConnectionProductsDB
    SQL.Strings = (
      'SELECT a.record_id, a.created_at, a.work, a.work_index, ('
      '  WITH RECURSIVE acc(record_id, parent_record_id, level) AS ('
      '    SELECT'
      '      record_id, parent_record_id, level'
      '    FROM work_log WHERE work_log.record_id = a.record_id'
      '    UNION'
      '    SELECT'
      '      w.record_id, w.parent_record_id, w.level'
      '    FROM acc'
      
        '      INNER JOIN work_log w ON w.parent_record_id = acc.record_i' +
        'd'
      '  )'
      '  SELECT'
      '    EXISTS( SELECT * FROM acc WHERE level >= 4)'
      ') as has_error, ('
      '   SELECT exists('
      '       SELECT * FROM  work_log b'
      
        '       WHERE b.work NOTNULL AND b.parent_record_id = a.record_id' +
        ' )'
      ') as has_children'
      'FROM work_log a'
      'WHERE a.parent_record_id = :parent_record_id  AND work NOTNULL ;')
    Left = 88
    Top = 188
    ParamData = <
      item
        Name = 'PARENT_RECORD_ID'
        ParamType = ptInput
      end>
  end
  object FDQuery1: TFDQuery
    Connection = FDConnectionProductsDB
    Left = 152
    Top = 260
  end
  object FDQueryConfig2: TFDQuery
    Connection = FDConnectionConfig
    Left = 152
    Top = 364
  end
  object FDQueryPartyWorks: TFDQuery
    Connection = FDConnectionProductsDB
    SQL.Strings = (
      'SELECT a.record_id, a.created_at, a.work, a.work_index, ('
      '  WITH RECURSIVE acc(record_id, parent_record_id, level) AS ('
      '    SELECT'
      '        record_id, parent_record_id, level'
      '    FROM work_log WHERE work_log.record_id = a.record_id'
      '    UNION'
      '    SELECT'
      '        w.record_id, w.parent_record_id, w.level'
      '    FROM acc'
      
        '      INNER JOIN work_log w ON w.parent_record_id = acc.record_i' +
        'd'
      '  )'
      '  SELECT'
      '      EXISTS( SELECT * FROM acc WHERE level >= 4)'
      ') as has_error, ('
      '         SELECT exists('
      '                    SELECT * FROM  work_log b'
      
        '                    WHERE b.work NOTNULL AND b.parent_record_id ' +
        '= a.record_id )'
      '       ) as has_children'
      'FROM work_log a'
      'WHERE'
      '  a.party_id = :party_id AND'
      '  a.parent_record_id ISNULL  AND'
      '  work NOTNULL AND'
      '    cast(strftime('#39'%Y'#39', a.created_at) AS INT) = :year AND'
      '    cast(strftime('#39'%m'#39', a.created_at) AS INT) = :month AND'
      '    cast(strftime('#39'%d'#39', a.created_at) AS INT) = :day;')
    Left = 552
    Top = 184
    ParamData = <
      item
        Name = 'PARTY_ID'
        ParamType = ptInput
      end
      item
        Name = 'YEAR'
        ParamType = ptInput
      end
      item
        Name = 'MONTH'
        ParamType = ptInput
      end
      item
        Name = 'DAY'
        ParamType = ptInput
      end>
  end
  object FDQueryWorkMessages: TFDQuery
    Connection = FDConnectionProductsDB
    SQL.Strings = (
      'WITH RECURSIVE acc(record_id, parent_record_id) AS ('
      '  SELECT'
      '    record_id, parent_record_id'
      '  FROM work_log WHERE work_log.record_id = :record_id'
      '  UNION'
      '  SELECT'
      '    w.record_id, w.parent_record_id'
      '  FROM acc'
      '    INNER JOIN work_log w ON w.parent_record_id = acc.record_id'
      ')'
      'SELECT'
      '  l.created_at as created_at,'
      '  l.product_serial as product_serial,'
      '  p.work_index as work_index,'
      '  l.level as level,'
      '  l.message as message'
      'FROM acc'
      '  INNER JOIN work_log p ON acc.parent_record_id = p.record_id'
      '  INNER JOIN work_log l ON acc.record_id = l.record_id'
      'WHERE l.message NOT NULL AND l.level NOT NULL;')
    Left = 224
    Top = 112
    ParamData = <
      item
        Name = 'RECORD_ID'
        ParamType = ptInput
      end>
  end
  object FDConnectionConfig: TFDConnection
    Params.Strings = (
      'Database=$(MYAPPDATA)\'#1040#1085#1072#1083#1080#1090#1087#1088#1080#1073#1086#1088'\ankat\config.db'
      'LockingMode=Normal'
      'Synchronous=Full'
      'JournalMode=WAL'
      'DriverID=SQLite')
    UpdateOptions.AssignedValues = [uvLockWait]
    UpdateOptions.LockWait = True
    Connected = True
    Left = 248
    Top = 24
  end
  object FDQueryConfig: TFDQuery
    Connection = FDConnectionConfig
    Left = 152
    Top = 308
  end
  object FDQueryConfig3: TFDQuery
    Connection = FDConnectionConfig
    Left = 144
    Top = 420
  end
  object FDQueryUpdateCoefValue: TFDQuery
    Connection = FDConnectionProductsDB
    SQL.Strings = (
      
        'INSERT OR REPLACE INTO product_coefficient_value (party_id, prod' +
        'uct_serial, coefficient_id, value)'
      'VALUES ((SELECT * FROM current_party_id),'
      
        '        (SELECT product_serial FROM current_party_products_confi' +
        'g WHERE ordinal = :ordinal),'
      '        :coef, :value);')
    Left = 264
    Top = 252
    ParamData = <
      item
        Name = 'ORDINAL'
        ParamType = ptInput
      end
      item
        Name = 'COEF'
        ParamType = ptInput
      end
      item
        Name = 'VALUE'
        ParamType = ptInput
      end>
  end
  object FDQueryDeleteCoefValue: TFDQuery
    Connection = FDConnectionProductsDB
    SQL.Strings = (
      'DELETE FROM product_coefficient_value WHERE'
      '    coefficient_id = :coef AND party_id IN current_party_id AND'
      '    product_serial IN ('
      
        '        SELECT product_serial FROM current_party_products_config' +
        ' WHERE ordinal = :ordinal'
      '    );')
    Left = 264
    Top = 316
    ParamData = <
      item
        Name = 'COEF'
        ParamType = ptInput
      end
      item
        Name = 'ORDINAL'
        ParamType = ptInput
      end>
  end
  object FDQueryPartyWorksDays: TFDQuery
    Connection = FDConnectionProductsDB
    SQL.Strings = (
      'SELECT DISTINCT'
      '    cast(strftime('#39'%Y'#39', created_at) AS INT) as year,'
      '    cast(strftime('#39'%m'#39', created_at) AS INT) as month,'
      '    cast(strftime('#39'%d'#39', created_at) AS INT) as day'
      'FROM work_log a'
      
        'WHERE a.party_id = :party_id AND a.parent_record_id ISNULL  AND ' +
        'work NOTNULL;')
    Left = 408
    Top = 176
    ParamData = <
      item
        Name = 'PARTY_ID'
        ParamType = ptInput
      end>
  end
  object FDQueryCurrentWorkMessages: TFDQuery
    Connection = FDConnectionProductsDB
    SQL.Strings = (
      'WITH RECURSIVE acc(record_id, parent_record_id) AS ('
      '  SELECT'
      '    record_id, parent_record_id'
      
        '  FROM last_work_log WHERE last_work_log.work_index = :work_inde' +
        'x'
      '  UNION'
      '  SELECT'
      '    w.record_id, w.parent_record_id'
      '  FROM acc'
      
        '    INNER JOIN last_work_log w ON w.parent_record_id = acc.recor' +
        'd_id'
      ')'
      'SELECT'
      '  l.created_at as created_at,'
      '  p.work_index as work_index,'
      '  l.level as level,'
      '  l.message as message,'
      '  l.product_serial AS product_serial'
      'FROM acc'
      
        '  INNER JOIN last_work_log p ON acc.parent_record_id = p.record_' +
        'id'
      '  INNER JOIN last_work_log l ON acc.record_id = l.record_id'
      'WHERE l.message NOT NULL AND l.level NOT NULL;')
    Left = 400
    Top = 104
    ParamData = <
      item
        Name = 'WORK_INDEX'
        ParamType = ptInput
      end>
  end
  object FDQueryPartyProductsWithCoefs: TFDQuery
    Connection = FDConnectionProductsDB
    SQL.Strings = (
      'SELECT DISTINCT a.product_serial'
      'FROM product a'
      
        '  INNER JOIN product_coefficient_value b ON a.party_id = b.party' +
        '_id and a.product_serial = b.product_serial'
      'WHERE a.party_id  = :party_id AND b.value NOTNULL;')
    Left = 544
    Top = 288
    ParamData = <
      item
        Name = 'PARTY_ID'
        ParamType = ptInput
      end>
  end
  object FDQueryPartyCoefsWithProducts: TFDQuery
    Connection = FDConnectionProductsDB
    SQL.Strings = (
      'SELECT DISTINCT b.coefficient_id'
      'FROM product a'
      
        '  INNER JOIN product_coefficient_value b ON a.party_id = b.party' +
        '_id and a.product_serial = b.product_serial'
      'WHERE a.party_id  = :party_id AND b.value NOTNULL;')
    Left = 552
    Top = 360
    ParamData = <
      item
        Name = 'PARTY_ID'
        ParamType = ptInput
      end>
  end
  object FDQueryDayLog: TFDQuery
    Connection = FDConnectionProductsDB
    SQL.Strings = (
      'SELECT'
      '  w.created_at AS created_at,'
      '  w.level AS level,'
      '  l.work AS work,'
      '  l.work_index AS work_index,'
      '  w.product_serial AS product_serial,'
      '  w.message AS message'
      'FROM work_log w'
      '  INNER JOIN work_log l on w.parent_record_id = l.record_id'
      'WHERE'
      '  w.message NOTNULL AND'
      '  cast(strftime('#39'%Y'#39', w.created_at) AS INT) = :year AND'
      '  cast(strftime('#39'%m'#39', w.created_at) AS INT) = :month AND'
      '  cast(strftime('#39'%d'#39', w.created_at) AS INT) = :day;')
    Left = 712
    Top = 360
    ParamData = <
      item
        Name = 'YEAR'
        ParamType = ptInput
      end
      item
        Name = 'MONTH'
        ParamType = ptInput
      end
      item
        Name = 'DAY'
        ParamType = ptInput
      end>
  end
  object FDQueryWorkLogYears: TFDQuery
    Connection = FDConnectionProductsDB
    SQL.Strings = (
      'SELECT DISTINCT cast(strftime('#39'%Y'#39', created_at) AS INT) as year'
      'FROM work WHERE parent_work_id ISNULL;')
    Left = 720
    Top = 124
  end
  object FDQueryWorkLogYearMonths: TFDQuery
    Connection = FDConnectionProductsDB
    SQL.Strings = (
      'SELECT DISTINCT cast(strftime('#39'%m'#39', created_at) AS INT) as month'
      'FROM work a'
      'WHERE cast(strftime('#39'%Y'#39', created_at) AS INT) = :year'
      'AND a.parent_work_id ISNULL;')
    Left = 712
    Top = 188
    ParamData = <
      item
        Name = 'YEAR'
        ParamType = ptInput
      end>
  end
  object FDQueryWorkLogYearMonthDays: TFDQuery
    Connection = FDConnectionProductsDB
    SQL.Strings = (
      'SELECT DISTINCT cast(strftime('#39'%d'#39', created_at) AS INT) as day'
      'FROM work'
      'WHERE'
      '    cast(strftime('#39'%Y'#39', created_at) AS INT) = :year AND'
      '    cast(strftime('#39'%m'#39', created_at) AS INT) = :month AND'
      '    parent_work_id ISNULL;')
    Left = 712
    Top = 244
    ParamData = <
      item
        Name = 'YEAR'
        ParamType = ptInput
      end
      item
        Name = 'MONTH'
        ParamType = ptInput
      end>
  end
  object FDQueryWorkLogsYearMonthDay: TFDQuery
    Connection = FDConnectionProductsDB
    SQL.Strings = (
      'SELECT a.record_id, a.created_at, a.work, a.work_index, ('
      '  WITH RECURSIVE acc(record_id, parent_record_id, level) AS ('
      '    SELECT'
      '        record_id, parent_record_id, level'
      '    FROM work_log WHERE work_log.record_id = a.record_id'
      '    UNION'
      '    SELECT'
      '        w.record_id, w.parent_record_id, w.level'
      '    FROM acc'
      
        '      INNER JOIN work_log w ON w.parent_record_id = acc.record_i' +
        'd'
      '  )'
      '  SELECT'
      '      EXISTS( SELECT * FROM acc WHERE level >= 4)'
      ') as has_error, ('
      '         SELECT exists('
      '                    SELECT * FROM  work_log b'
      
        '                    WHERE b.work NOTNULL AND b.parent_record_id ' +
        '= a.record_id )'
      '       ) as has_children'
      'FROM work_log a'
      '                                                     WHERE'
      '    a.parent_record_id ISNULL  AND'
      '    work NOTNULL AND'
      '    cast(strftime('#39'%Y'#39', a.created_at) AS INT) = :year AND'
      '    cast(strftime('#39'%m'#39', a.created_at) AS INT) = :month AND'
      '    cast(strftime('#39'%d'#39', a.created_at) AS INT) = :day;')
    Left = 336
    Top = 396
    ParamData = <
      item
        Name = 'YEAR'
        ParamType = ptInput
      end
      item
        Name = 'MONTH'
        ParamType = ptInput
      end
      item
        Name = 'DAY'
        ParamType = ptInput
      end>
  end
end
