function none = PrintQamSymbols()

    clc
    clear

    idealRMS = [];

    for bitAllocated = 5:5
        M = 2^bitAllocated;
        modObj = modem.qammod('M', M, 'SymbolOrder', 'Gray', 'InputType', 'Bit');
        bits = [];

        % for i = 0:M - 1
        %     bits = [bits, de2bi(i)];
        % end

        bits = de2bi(0:M - 1);

        bits = bits';

        QAMSymbols = modulate(modObj, bits)
        idealRMS = [idealRMS, rms(QAMSymbols)];
    end

    idealRMS
