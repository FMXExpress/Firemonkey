package com.blong.test;

import android.content.BroadcastReceiver;
import android.content.Intent;
import android.content.Context;
import android.util.Log;

public class ServiceReceiver extends BroadcastReceiver
{
	static final String TAG = "ServiceReceiver";

	public native void serviceReceiverOnReceiveNative(Context context, Intent receivedIntent);

	@Override
	public void onReceive(Context context, Intent receivedIntent)
	{
		Log.d(TAG, "onReceive");
		serviceReceiverOnReceiveNative(context, receivedIntent);
	}
}