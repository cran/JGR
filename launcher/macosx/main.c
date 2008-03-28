#include <mach-o/dyld.h>
#include <Carbon/Carbon.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "prefsp.h"
#include "javacf.h"

/* JGR Loader version should always parse to a real number */
#define JGR_LOADER_VERSION "1.61"
/* Loader generation can be requested in the DESCRIPTION of the JGR package */
#define JGR_LOADER_GENERATION 2

/* ChangeLog
   1.61  - SET_DEFAULT_PACKAGES (default unset) determines whether R_DEFAULT_PACKAGES will be set
           or not. To notify JGR jgr.load.pkgs property is set to "yes" if it's JGR's responsibility
           to load packages (default) or "no" if the loader loaded them using R_DEFAULT_PACKAGES.
   1.6
 */

#define DEFAULT_RHOME "/Library/Frameworks/R.framework/Resources"

/* find out the architecture of this loader and use it to set R_ARCH */
#ifdef __ppc__
#define arch_str "/ppc"
#elif defined __i386__
#define arch_str "/i386"
#elif defined __x86_64__
#define arch_str "/x86_64"
#elif defined __ppc64__
#define arch_str "/ppc64"
#elif defined __arm__
#define arch_str "/arm"
#endif

static char *rhome;
static char *libroot = 0;
static char *cran = "http://cran.r-project.org/";
/* char *cran = "http://www.rforge.net/"; */

static char *xmx = "-Xmx512m"; /* default memory setting */

/* from originally from the Windows launcher jgr.c */

FILE *f = 0; /* debug file - not used here */

/* returns package version of a given package - searches $R_HOME/library/ only
 given version string aa.bb-cc the returned long is 0xaabbcc (except that
 vs is treated as decimal number - i.e. 1.2-12 will return 0x01020c )
returns 0 if package doesn't exist or there is no Version entry */
long getPkgVersion(char *pkg) {
	char dfn[1024];
	FILE *pf;
	long ver=-1;
	
	strcpy(dfn,libroot);
	strcat(dfn,pkg);
	strcat(dfn,"/DESCRIPTION");
	pf = fopen(dfn,"r");
	if (!pf) { if (f) fprintf(f,"getPkgVersion(%s): not found\n",pkg); return 0; }
	dfn[1023]=0;
	while (fgets(dfn, 1023,pf)) {
		if (!strncmp(dfn,"RequiredLauncherGeneration:",27)) {
			char *c = dfn+27; while (*c==' ' || *c=='\t') c++;
			int lg = atoi(c);
			if (f) fprintf(f, "required launcher generation in pkg '%s' is %d (this launcher is %d)\n",
						   pkg, lg, JGR_LOADER_GENERATION);
			if (lg > JGR_LOADER_GENERATION) { /* launcher is too old */
				if (f) fprintf(f, "*** Launcher too old, aborting (get a new one from http://www.rforge.net/JGR/)\n");
				system("echo 'tell application \"Finder\"~activate~display dialog \"This JGR launcher is out of date.~~Your JGR package requires a more recent launcher. Please download the latest version from~http://www.rosuda.org/JGR/\" buttons {\"OK\"} default button 1~end tell'|sed 'y/~/\\n/'|osascript -");
				exit(1);
			}
		}
		if (!strncmp(dfn,"Version:",8)) {
			char *c = dfn+8;
			while (*c && (*c==' ' || *c=='\t')) c++;
			if (*c) {
				char *d1=c;
				while (*c && *c!='.') c++;
				if (*c) {
					char *d2=c+1;
					*c=0; c++;
					while (*c && *c!='-') c++;
					if (*c) {
						char *d3=c+1;
						*c=0; c++;
						while (*c>='0' && *c<='9') c++;
						*c=0;
						if (f) fprintf(f,"getPkgVersion(%s): %06x\n", pkg, (atoi(d3)&0xff)|((atoi(d2)&0xff)<<8)|((atoi(d1)&0xff)<<16));
						ver = (atoi(d3)&0xff)|((atoi(d2)&0xff)<<8)|((atoi(d1)&0xff)<<16);
					}
				}
			}
		}
	}
	if (ver>0) {
		fclose(pf);
		return ver;
	}
	if (f) fprintf(f,"getPkgVersion(%s): version info not found\n",pkg);
	fclose(pf);
	return 0;
}

/* the main stuff */

static char buf[8192], tbuf[1024], jrilib[2048], npkg[512];
static int debugLevel=0;
static struct stat sts;

#define userArgs 24

int main(int argc, char* argv[])
{
	CFStringRef urls=0;
	int urll=0;
	int pass=0;
	int i;
	CFBundleRef mb = CFBundleGetMainBundle();
	char *dylib=getenv("DYLD_LIBRARY_PATH");
	char *urlcs;
	char *clspath, *bootpath;
	char *drJavaPath;
	char **prefs;
	char *rhomerequest=0;
#if SET_DEFAULT_PACKAGES
	int  setDefPkg=0;
#endif
	
	char *jargv[64];
	int jargc = userArgs;
	
	{
		char *c = getenv("CRAN");
		if (c && *c) cran = c;
	}
	
	if (!mb) {
		fprintf(stderr, "JGR FATAL: Can't get main bundle!\n");
		return -1;
	} else {
		CFURLRef rdurl = CFBundleCopyBundleURL(mb);
		if (!rdurl) {
			fprintf(stderr, "JGR FATAL: Can't get main bundle URL!\n");
			return -1;
		}
		urls = CFURLCopyFileSystemPath(rdurl,kCFURLPOSIXPathStyle);
		if (!urls) {
			fprintf(stderr, "JGR FATAL: Can't get main bundle path!\n");
			return -1;
		}
		CFRelease(rdurl);
		CFRelease(mb);
	}
	
	if (!CFStringGetCString(urls, buf, 1024, kCFStringEncodingUTF8)) {
		fprintf(stderr, "JGR FATAL: Can't store main bundle path!\n");
		return -1;
	}
	urll=strlen(buf);
	urlcs=(char*)malloc(urll+1);
	strcpy(urlcs, buf);
	
	if (dylib) strcpy(buf,dylib); else *buf=0;
	
	/* R_LIBS: library paths, colon-separated */
	strcpy(buf,getenv("HOME"));
	strcat(buf,"/.JGRprefsrc");
	printf("Loading preferences from %s\n", buf);
	prefs=parse_prefs_file(buf);
	i=0;
	while (prefs && prefs[i]) {
		char *key=prefs[i++];
		char *val=0;
		if (!prefs[i]) break;
		val=prefs[i++];
#if SET_DEFAULT_PACKAGES
		if (!strcmp(key,"DefaultPackages") && val) {
			char *c=val, *d=val;
			while (*c) {
				if (*c!=' ' && *c!='\t') { *d=*c; d++; }
				c++;
			}
			*d=0;
			/*if (!strstr(val, "JGR")) {
				char *c=(char*) malloc(strlen(val)+5);
				strcpy(c, val);
				strcat(c, ",JGR");
				val=c;
			}*/
			printf("set (from prefs): R_DEFAULT_PACKAGES=\"%s\"\n", val);
			setenv("R_DEFAULT_PACKAGES", val, 1);
			setDefPkg=1;
		}
#endif
#if 0
		if (!strcmp(key,"AdditionalRLibraryPath") && val) {
			printf("set (from prefs): R_LIBS=\"%s\"\n", val);
			setenv("R_LIBS", val, 1);
		}
#endif
		if (!strcmp(key,"InitialRLibraryPath") && val) {
			printf("set (from prefs): R_LIBS=\"%s\"\n", val);
			setenv("R_LIBS", val, 1);
		}
		if (!strcmp(key,"InitialRHome") && val) {
			printf("from prefs: R_HOME=\"%s\"\n", val);
			rhomerequest=val;
		}
		if (!strcmp(key,"DebugLevel") && val && *val) {
			debugLevel=atoi(val);
			printf("DEBUG level set to %d.\n", debugLevel);
		}
		if (!strcmp(key,"CmdArg") && val) {
			if (jargc<64)
				jargv[jargc++]=val;
		}
	}
		
	setenv("R_HOME",(rhomerequest)?rhomerequest:DEFAULT_RHOME,(rhomerequest)?1:0);
	rhome=getenv("R_HOME");
    
	printf("JGR R_HOME: %s\n", rhome);
	
	libroot = getenv("R_LIBS");
	if (libroot && *libroot) {
		char *c = (char*) malloc(strlen(libroot)+3);
		strcpy(c, libroot);
		libroot = c;
		while (*c && *c!=':') c++;
		*c = 0;
		if (*(c-1) != '/') { *c='/'; c[1]=0; }
	}
	if (!libroot) {
		libroot=(char*) malloc(strlen(rhome)+16);
		strcpy(libroot, rhome);
		strcat(libroot, "/library/");
	}
	printf("libroot: %s\n", libroot);
	
	/* check if JGR exists and support packages are installed */
chkJGRpkg:
	*npkg=0;
	pass++;
	
	/* requires JGR 1.6-0 or higher */
	if (getPkgVersion("JGR")<0x10600) strcat(npkg,"\"JGR\",");
	/* requires rJava 0.5-0 or higher */
	if (getPkgVersion("rJava")<0x500) strcat(npkg,"\"rJava\",");
	/* requires JavaGD 0.4-2 or higher */
	if (getPkgVersion("JavaGD")<0x402) strcat(npkg,"\"JavaGD\",");
	/* requires iplots 1.1-2 or higher */
	if (getPkgVersion("iplots")<0x10102) strcat(npkg,"\"iplots\",");

#if 0
	if (*npkg) { /* we won't install iWidgets on its own, but as a part of the full install - why not? */
		if (getPkgVersion("iWidgets")<0x105) strcat(npkg,"\"iWidgets\",");
	}
#endif
	if (*npkg) npkg[strlen(npkg)-1]=0;
	
	if (strlen(npkg)>0) {
		char *c, *d=npkg;
		int doit=0;
		
		if (pass>1) {
			system("echo 'tell application \"Finder\"~activate~display dialog \"Package installation is still incomplete. Please try again or install all necessary packages manually.\" buttons {\"OK\"} default button 1~end tell'|sed 'y/~/\\n/'|osascript -");
			exit(2);
		}
		
		strcpy(tbuf,"echo 'tell application \"Finder\"~activate~display dialog \"The following packages need to be installed or updated for JGR to run properly: ");
		c=tbuf+strlen(tbuf);
		while (*d) { if (*d!='"') { *c=*d; c++; }; d++; }
		strcpy(c,". I will run R to install those packages - you need properly configured internet connection for this to work, otherwise you will have to install the packages manually.\" buttons {\"Cancel\",\"OK\"} default button 2~end tell'|sed 'y/~/\\n/'|osascript - > /tmp/JGRlaunch.res");
		system("rm -f /tmp/JGRlaunch.res");
		system(tbuf);
		{
			FILE *rf=fopen("/tmp/JGRlaunch.res","r");
			if (rf) {
				if (fgets(tbuf,1023,rf) && strstr(tbuf,":OK")) doit=1;
				fclose(rf);
			}
		}
		system("rm -f /tmp/JGRlaunch.res");
		printf("User requested installation of %s? %s\n", npkg, doit?"yes":"no");
		if (doit) {
			char *c=tbuf, *d=npkg; /* we need to escape " */
			while (*d) { if (*d=='"') { *c='\\'; c++; *c='\\'; c++; *c='\\'; c++;}; *c=*d; c++; d++; };
			*c=0; strcpy(npkg, tbuf);
			snprintf(tbuf, 1023, "echo \"tell application \\\"Terminal\\\"~activate~do script \\\"echo 'install.packages(c(%s),\\\\\\\"%s\\\\\\\",c(\\\\\\\"%s\\\\\\\"))'|%s/bin/R --no-save --vanilla --slave; echo done>/tmp/JGRpkg.inst\\\"~end tell\"|sed 'y/~/\\n/'|osascript -", npkg, libroot, cran, rhome);
			puts(tbuf);
			system("rm -f /tmp/JGRpkg.inst");
			system(tbuf);
			{
				int maxwait=300;
				while (maxwait) {
					sleep(1);
					if (!stat("/tmp/JGRpkg.inst",&sts)) break;
					maxwait--;
				}
			}
			system("rm -f /tmp/JGRpkg.inst");			
			goto chkJGRpkg;
		} else {
			exit(1);
		}
	}
	
#if SET_DEFAULT_PACKAGES
	if (!setDefPkg)	setenv("R_DEFAULT_PACKAGES", "utils,grDevices,graphics,stats,methods", 0);
#endif
	
	if (!getenv("R_DOC_DIR")) {
		strcpy(buf,rhome);	strcat(buf,"/doc");	setenv("R_DOC_DIR",buf,1);
	}
	if (!getenv("R_INCLUDE_DIR")) {
		strcpy(buf, rhome); strcat(buf, "/include"); setenv("R_INCLUDE_DIR", buf, 1);
	}
	if (!getenv("R_SHARE_DIR")) {
		strcpy(buf, getenv("R_HOME")); strcat(buf, "/share"); setenv("R_SHARE_DIR", buf, 1);
	}

	*buf=0;
	if (dylib && *dylib) { strcpy(buf, dylib); strcat(buf, ":"); }
//	strcpy(buf,"/lib:/usr/lib:"); /* we need to prepend system libraries, otherwise out gcc4 libs override the system - bad idea */

	/* see if have all JAR dependencies that we need */
	strcpy(tbuf, libroot);
	strcat(tbuf, "rJava/jri/JRI.jar");
	if (stat(tbuf, &sts)) {
		printf("Cannot stat %s - JRI jar file, displaying error message and closing.\n", tbuf);
		system("echo 'tell application \"Finder\"~activate~display dialog \"Cannot find rJava/JRI Java classes. Please make sure that the latest rJava R package is correctly installed.\" buttons {\"OK\"} default button 1~end tell'|sed 'y/~/\\n/'|osascript -");
		ExitToShell();		
	}
	strcpy(tbuf, libroot);
	strcat(tbuf, "iplots/java/iplots.jar");
	if (stat(tbuf, &sts)) {
	   printf("Cannot stat %s - iPlots jar file, displaying error message and closing.\n", tbuf);
	   system("echo 'tell application \"Finder\"~activate~display dialog \"Cannot find iplots Java classes. Please make sure that the latest iplots R package is correctly installed.\" buttons {\"OK\"} default button 1~end tell'|sed 'y/~/\\n/'|osascript -");
	   ExitToShell();		
	}

	/* try to find libjri in rJava */
	strcpy(tbuf, libroot);
	strcat(tbuf,"rJava/jri/libjri.jnilib");
	printf("Attempting to load jnilib: %s\n", tbuf);
	/* check whether the libjri is also functional */
	const struct mach_header* mh = NSAddImage(tbuf, NSADDIMAGE_OPTION_RETURN_ON_ERROR|NSADDIMAGE_OPTION_WITH_SEARCHING);
	if (!mh) {
	   NSLinkEditErrors er;
	   int errorNumber;
	   const char *fn;
	   const char *err;
	   NSLinkEditError(&er, &errorNumber, &fn, &err);
	   strcpy(buf,"echo 'tell application \"Finder\"~activate~display dialog \"Cannot load JRI dynamic library. Please make sure latest version of rJava R package is installed. Cause: ");
	   strcat(buf,err);
	   strcat(buf,"\" buttons {\"OK\"} default button 1~end tell'|sed 'y/~/\\n/'|osascript -");
	   system(buf);
	   return 1;
	}
	
	printf("Succeeded in loading jnilib, looks good.\n");
	tbuf[strlen(tbuf)-14]=0; /* remove /libjri.jnilib from the path */
	strcpy(jrilib, tbuf);
	strcat(buf,tbuf);
	setenv("DYLD_LIBRARY_PATH",buf,1);
	printf("JGR DYLD_LIBRARY_PATH: %s\n", buf);

	strcpy(buf, "-Drjava.path=");
	strcat(buf, libroot);
	strcat(buf, "rJava");
	drJavaPath = strdup(buf);

	strcpy(buf, libroot);
	strcat(buf, "rJava/java/boot");
	bootpath = strdup(buf);

	chdir(getenv("HOME"));
	
	strcpy(buf,"-Drjava.class.path=");
	if (getenv("CLASSPATH")) {
		strcat(buf,getenv("CLASSPATH"));
		strcat(buf,":");
	}
	
	printf("Loading class file...\n");
	printf("result %d\n", load_R_java_class_file());
	char *rcp=get_class_path();
	printf("RCP:%s\n", rcp?rcp:"<none>");
	if (rcp && *rcp) {
		strcat(buf,rcp);
		strcat(buf,":");
		free(rcp);
	}
	
	strcat(buf,urlcs);  /* java class file */
	strcat(buf,":");
	strcat(buf, libroot); /* JRI */
	strcat(buf, "rJava/jri/JRI.jar:");
	strcat(buf, libroot); /* iplots */
	strcat(buf, "iplots/java/iplots.jar:");
	strcat(buf, libroot);  /* JGR */
	strcat(buf, "JGR/java/JGR.jar");
	
	printf("JGR CLASSPATH: %s\n", buf);
	
	clspath=strdup(buf);
	
	strcpy(buf,"-Xdock:icon=");
	strcat(buf,urlcs);
	strcat(buf,"/Contents/Resources/JGR.icns");

#ifdef arch_str
	if (!getenv("R_ARCH")) {
		char fb2[768];
		strcpy(fb2,rhome);
		strcat(fb2,"/lib");
		strcat(fb2,arch_str);
		if (stat(fb2, &sts)==0)
			setenv("R_ARCH", arch_str, 1);
	}
#else
#warning "Unknown architecture, R_ARCH won't be set automatically."
#endif
	printf("R_ARCH=%s\n", getenv("R_ARCH"));
	
	strcpy(tbuf,libroot);
	strcat(tbuf,"JGR/java/JGR.jar");

	if (stat(tbuf, &sts)) {
		printf("Cannot stat %s - JGR as R package is not installed, displaying error message and closing.\n", tbuf);
		system("echo 'tell application \"Finder\"~activate~display dialog \"Cannot find JGR Java classes. Please make sure that the JGR package is correctly installed.\" buttons {\"OK\"} default button 1~end tell'|sed 'y/~/\\n/'|osascript -");
		/*
		 DialogRef theAlert;
		CreateStandardAlert(kAlertStopAlert, CFSTR("Cannot find JGR class files. Plesae make sure that the JGR package is installed correctly."), NULL, NULL, &theAlert);
		RunStandardAlert(theAlert, NULL, NULL);  */
		ExitToShell();
	}
	
	{
		int argb=userArgs-1;
		if (debugLevel>0) jargv[argb--]="--debug";
		jargv[argb--]="RJavaClassLoader";
		jargv[argb--]=drJavaPath;
		jargv[argb--]=clspath;
#if SET_DEFAULT_PACKAGES
		jargv[argb--]="-Djgr.load.pkgs=no";
#else
		jargv[argb--]="-Djgr.load.pkgs=yes";
#endif
		jargv[argb--]="-Djgr.loader.ver=" JGR_LOADER_VERSION;
		jargv[argb--]="-Dmain.class=org.rosuda.JGR.JGR";
		jargv[argb--]="-Dcom.apple.mrj.application.apple.menu.about.name=JGR";
		jargv[argb--]="-Dapple.laf.useScreenMenuBar=true";
		/* strcpy(tbuf,"-Djava.library.path="); strcat(tbuf, getenv("DYLD_LIBRARY_PATH")); jargv[argb--]=tbuf; */
		jargv[argb--]=buf;
		jargv[argb--]=xmx;
		jargv[argb--]=bootpath;
		jargv[argb--]="-cp";
		/* final: program name */
		jargv[argb]="java";
		jargv[jargc]=NULL;
		
		{
			int ii=argb;
			printf("arguments to java:\n");
			while (ii<jargc) printf("  %s\n", jargv[ii++]);
		}
		
		execvp("java",jargv+argb);
		/* execlp("java","java","-cp",clspath,"-Xmx512m",buf,"-Dapple.laf.useScreenMenuBar=true","-Dcom.apple.mrj.application.apple.menu.about.name=JGR","org.rosuda.JGR.JGR",(debugLevel>0)?"--debug":0,NULL); */
	}
    fprintf(stderr, "Can't start Java engine!\n");
    return -1;
}
