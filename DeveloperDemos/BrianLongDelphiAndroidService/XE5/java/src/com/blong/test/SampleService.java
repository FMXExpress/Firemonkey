package com.blong.test;

import android.app.Service;
import android.content.Intent;
import android.os.IBinder;
import android.util.Log;

public class SampleService extends Service
{
	static final String TAG = "SampleService";

	public SampleService() 
	{
		super();
		Log.d(TAG, "Constructor");
	}

	public native void sampleServiceOnDestroyNative();

	@Override
    public void onDestroy()
    {
		Log.d(TAG, "onDestroy");
		try
		{
			sampleServiceOnDestroyNative();
		}
		catch (java.lang.UnsatisfiedLinkError e)
		{
			Log.d(TAG, "Native onDestroy implementation is missing. Either the app was built wrong or it is exiting.");
			//nothing
		}
    }
    
	@Override
    public IBinder onBind(Intent bindingIntent)
    {
		Log.d(TAG, "onBind");
		return null;
    }
    
	public native int sampleServiceOnStartCommandNative(Intent startIntent, int flags, int startID);

	@Override
	public int onStartCommand(Intent startIntent, int flags, int startID)
    {
		Log.d(TAG, "onStartCommand");
		try
		{
			return sampleServiceOnStartCommandNative(startIntent, flags, startID);
		}
		catch (java.lang.UnsatisfiedLinkError e)
		{
			Log.d(TAG, "Native onStartCommand implementation is missing. Either the app was built wrong or it is exiting.");
			return START_STICKY;
		}
    }
}