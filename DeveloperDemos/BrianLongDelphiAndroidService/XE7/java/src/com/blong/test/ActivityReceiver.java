package com.blong.test;

import android.content.BroadcastReceiver;
import android.content.Intent;
import android.content.Context;
import android.util.Log;

public class ActivityReceiver extends BroadcastReceiver
{
	static final String TAG = "ActivityReceiver";

	public native void activityReceiverOnReceiveNative(Context context, Intent receivedIntent);

	@Override
	public void onReceive(Context context, Intent receivedIntent)
	{
		Log.d(TAG, "onReceive");
		activityReceiverOnReceiveNative(context, receivedIntent);
	}
}