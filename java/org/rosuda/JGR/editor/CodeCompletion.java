package org.rosuda.JGR.editor;

import org.rosuda.JGR.JGR;

import javax.swing.*;
import java.awt.*;
import java.util.Collections;
import java.util.Vector;

public class CodeCompletion extends JPanel {

    public static final int C_HEIGHT = 100;

    public static final int C_WIDTH = 160;

    private static final long serialVersionUID = 2527975358381656662L;
    private static CodeCompletion instance = null;
    private String pattern = null;
    private Vector completions = JGR.KEYWORDS;
    private Vector _completions = new Vector();

    private JList completionsList = new JList();

    private CodeCompletion() {
        this.setMinimumSize(new Dimension(C_WIDTH, C_HEIGHT));
        this.setPreferredSize(new Dimension(C_WIDTH, C_HEIGHT));
        this.setMaximumSize(new Dimension(C_WIDTH, C_HEIGHT));
        this.setLayout(new GridLayout(1, 1));
        this.add(new JScrollPane(completionsList));
    }

    public static CodeCompletion getInstance() {
        if (instance == null) {
            instance = new CodeCompletion();
        }
        return instance;
    }

    public int updateList(String pattern) {
        this.pattern = pattern;
        _completions.clear();
        for (int i = 0; i < completions.size(); i++) {
            String s = (String) completions.get(i);
            if (s.startsWith(pattern)) {
                _completions.add(s);
            }
        }
        Collections.sort(_completions);
        completionsList.setListData(_completions);
        completionsList.setSelectedIndex(0);

        return _completions.size();
    }

    public String getCompletion() {
        String val = completionsList.getSelectedValue().toString();
        if (val != null) {
            return val.replaceFirst(this.pattern, "");
        }
        return "";
    }

    public int updateFileList(String pattern) {
        this.pattern = pattern;
        return -1;
    }

    public void previous() {
        int cSel = completionsList.getSelectedIndex();
        if (cSel > 0) {
            completionsList.setSelectedIndex(--cSel);
            completionsList.ensureIndexIsVisible(cSel);
        }
    }

    public void next() {
        int cSel = completionsList.getSelectedIndex();
        if (cSel < completionsList.getModel().getSize() - 1) {
            completionsList.setSelectedIndex(++cSel);
            completionsList.ensureIndexIsVisible(cSel);
        }
    }
}